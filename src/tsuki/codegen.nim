import std/strutils
import std/tables

import ast
import chunk
import common
import errors
import lexer
import symbol
import value

type
  Scope = object
    syms: Table[string, Symbol]
    vars, totalVars: int
    isPseudoscope: bool
      # pseudoscopes are a way of creating variables for values that should
      # be left on the stack. a pseudoscope does not pop its variables once
      # it's finished.

  FlowBlockKind = enum
    fbkLoopOuter
    fbkLoopIteration
    fbkProcBody

  FlowBlock = object
    kind: FlowBlockKind
    breaks: seq[int]
    bottomScope: int

  CodeGen* = ref object
    cs: CompilerState
    a: Assembly
    module: Module
    chunk: Chunk

    scopes: seq[Scope]
    flowBlocks: seq[FlowBlock]

    self: Symbol
      # the self variable, nil if the CodeGen isn't a proc generator inside of
      # an object
    result: Symbol
      # the result variable, nil if the CodeGen is not a proc generator
    objectType: Symbol
      # the current object type being implemented. nil if the CodeGen is
      # outside of an impl block

  CompileError* = object of ValueError
    filename*: FilenameId
    lineInfo*: LineInfo

  CompileErrorRef = ref CompileError


# init

proc newCodeGen*(cs: CompilerState, assembly: Assembly,
                 module: Module, chunk: Chunk): CodeGen =
  ## Creates and initializes a new codegen instance.

  CodeGen(
    cs: cs, a: assembly,
    module: module, chunk: chunk
  )

proc createSub(g: CodeGen): CodeGen =
  ## Creates a sub-codegen of the given codegen.

  CodeGen(
    cs: g.cs, a: g.a,
    module: g.module,
    objectType: g.objectType,
  )


# helpers

proc error(g: CodeGen, node: Node, message: string) =
  ## Raises a compilation error.

  raise CompileErrorRef(
    msg: errorFormat % [
      g.cs.getFilename(node.filename),
      $node.lineInfo.line, $node.lineInfo.column,
      message,
    ],
    filename: node.filename,
    lineInfo: node.lineInfo,
  )

proc assert(g: CodeGen, cond: bool, node: Node, message: string) =
  ## Raises a compilation error if cond is false.

  if not cond:
    g.error(node, message)

proc scope(g: CodeGen): var Scope =
  ## Returns the current scope.
  g.scopes[^1]

proc pushScope(g: CodeGen, pseudoscope = false) =
  ## Pushes a new scope.

  let totalVars =
    if g.scopes.len > 0: g.scopes[^1].totalVars
    else: 0
  g.scopes.add Scope(totalVars: totalVars, isPseudoscope: pseudoscope)

proc popScope(g: CodeGen) =
  ## Pops the current scope, together with its variables.

  let scope = g.scopes.pop()
  if not scope.isPseudoScope and scope.vars > 0:
    g.chunk.emitOpcode(opcDiscard)
    g.chunk.emitU8(scope.vars.uint8)

template withNewScope(g: CodeGen, body: untyped) =
  ## Helper for automatically pushing and popping a scope.

  g.pushScope()
  `body`
  g.popScope()

template withNewPseudoscope(g: CodeGen, body: untyped) =
  ## Helper for automatically pushing and popping a pseudoscope.

  g.pushScope(pseudoscope = true)
  `body`
  g.popScope()

proc pushFlowBlock(g: CodeGen, kind: FlowBlockKind) =
  ## Pushes a new flow block, and an associated scope.

  g.pushScope()
  g.flowBlocks.add FlowBlock(kind: kind, bottomScope: g.scopes.len)

proc popFlowBlock(g: CodeGen) =
  ## Pops the current flow block, and its associated scope..

  for hole in g.flowBlocks[^1].breaks:
    g.chunk.patchJump(hole)
  discard g.flowBlocks.pop()
  g.popScope()

proc breakFlowBlock(g: CodeGen, kind: FlowBlockKind): bool =
  ## Breaks the flow block with the matching kind.
  ## Returns true if a block was successfully broken out of, or false if a
  ## matching block could not be found.

  for i in countdown(g.flowBlocks.high, 0):
    let flow = g.flowBlocks[i]
    if flow.kind == kind:
      var varCount: int
      for i in countdown(g.scopes.high, flow.bottomScope):
        # i think this for loop can be avoided but i haven't figured out how.
        let scope = g.scopes[i]
        inc varCount, scope.vars
      if varCount > 0:
        g.chunk.emitOpcode(opcDiscard)
        g.chunk.emitU8(uint8 varCount)
      g.flowBlocks[i].breaks.add g.chunk.emitJump(opcJumpFwd)
      return true

template withNewFlowBlock(g: CodeGen, kind: FlowBlockKind, body: untyped) =
  ## Helper for automatically pushing and popping a flow block.

  g.pushFlowBlock(kind)
  `body`
  g.popFlowBlock()

proc lookupSymbol(g: CodeGen, name: Node): Symbol =
  ## Looks up a symbol in context of the current scope.
  ## Raises an error if the symbol cannot be found.

  let strName = name.stringVal

  # local scopes, top to bottom
  for i in countdown(g.scopes.high, 0):
    if strName in g.scopes[i].syms:
      result = g.scopes[i].syms[strName]
      return

  # module scope
  if strName in g.module.globalSyms:
    result = g.module.globalSyms[strName]

  g.assert(result != nil, name, ceSymUndeclared % strName)

proc addSymbol(g: CodeGen, sym: Symbol) =
  ## Adds a symbol to the topmost scope.

  var
    name = sym.name.stringVal
    tab: ptr Table[string, Symbol]

  if g.scopes.len > 0:
    tab = addr g.scope.syms
  else:
    tab = addr g.module.globalSyms

  g.assert(name notin tab[], sym.name, ceSymAlreadyDeclared % name)
  tab[][name] = sym

proc defineVar(g: CodeGen, name: Node): Symbol =
  ## Defines a new variable with the given name and returns its symbol.
  ## To set the value, use ``popToVar``.

  # local variable
  if g.scopes.len > 0:
    result = newVarSymbol(name, isLocal = true)
    result.stackPos = g.scope.totalVars
    inc g.scope.vars
    inc g.scope.totalVars

  # global variable
  else:
    result = newVarSymbol(name, isLocal = false)
    result.globalId = g.a.globalVarCount
    inc g.a.globalVarCount

  assert result != nil
  g.addSymbol(result)

proc pushVar(g: CodeGen, sym: Symbol) =
  ## Pushes a copy of the variable's value onto the stack.

  assert sym.kind == skVar
  if sym.isLocalVar:
    g.chunk.emitOpcode(opcPushLocal)
    g.chunk.emitU16(sym.stackPos.uint16)
  else:
    g.chunk.emitOpcode(opcPushGlobal)
    g.chunk.emitU16(sym.globalId.uint16)

proc popToVar(g: CodeGen, sym: Symbol, noAssign = false) =
  ## Pops the value at the top of the stack to the variable represented by the
  ## given symbol.
  ## ``noAssign`` controls whether ``opcPopToLocal`` should be emitted instead
  ## of ``opcAssignToLocal``.

  assert sym.kind == skVar
  if sym.isLocalVar:
    if sym.isSet:
      if noAssign:
        g.chunk.emitOpcode(opcPopToLocal)
      else:
        g.chunk.emitOpcode(opcAssignToLocal)
      g.chunk.emitU16(sym.stackPos.uint16)
  else:
    if sym.isSet:
      g.chunk.emitOpcode(opcAssignToGlobal)
    else:
      g.chunk.emitOpcode(opcPopToGlobal)
    g.chunk.emitU16(sym.globalId.uint16)
  sym.isSet = true

proc lineInfoFrom(g: CodeGen, n: Node) =
  ## Sets the node from which to use line info when emitting code.
  g.chunk.setCurrentLineInfo(n.lineInfo)


# AST → bytecode processing

proc genLiteral(g: CodeGen, n: Node) =
  ## Generates code for a literal node.

  g.lineInfoFrom(n)

  case n.kind
  of nkNil: g.chunk.emitOpcode(opcPushNil)
  of nkTrue: g.chunk.emitOpcode(opcPushTrue)
  of nkFalse: g.chunk.emitOpcode(opcPushFalse)
  of nkFloat:
    g.chunk.emitOpcode(opcPushFloat)
    g.chunk.emitFloat(n.floatVal)
  of nkString:
    g.chunk.emitOpcode(opcPushString)
    g.chunk.emitString(n.stringVal)
  else: unreachable "node kind must be a literal"

proc genStmt(g: CodeGen, n: Node)
proc genExpr(g: CodeGen, n: Node)

proc genStmtList(g: CodeGen, n: Node, isExpr: bool,
                 exprResultVar: Symbol = nil) =
  ## Generates code for a statement list.
  ## ``isExpr`` specifies whether the statement list is part of an expression.
  ## This enforces the last statement in the list to always be an expression.
  ## If ``isExpr`` is true, ``exprResultVar`` must not be nil, and is used as
  ## a result variable for the statement list.

  assert n.kind == nkStmtList
  g.lineInfoFrom(n)

  if isExpr:
    assert not exprResultVar.isNil,
      "the result variable must always be provided for expression StmtLists"
    assert exprResultVar.isLocalVar,
      "the result variable must be a local to have stack semantics"
    assert exprResultVar.isSet,
      "the result variable must be set to be sure that popToVar will pop " &
      "to the local instead of just leaving it there"
    for i, stmt in n:
      if i < n.high:
        g.genStmt(stmt)
      else:
        g.genExpr(stmt)
        g.popToVar(exprResultVar, noAssign = true)
  else:
    for stmt in n:
      g.genStmt(stmt)

proc genVarLookup(g: CodeGen, n: Node) =
  ## Generates code for a variable lookup.

  assert n.kind == nkIdent
  g.lineInfoFrom(n)

  let sym = g.lookupSymbol(n)
  g.assert(sym.kind == skVar, n, ceSymIsNotAVariable % sym.name.stringVal)
  g.pushVar(sym)

proc genAssignment(g: CodeGen, left, right: Node) =
  ## Generates code for an assignment. Part of ``genSpecialInfix``.

  case left.kind

  of nkIdent:
    # assignment to variable
    let sym = g.lookupSymbol(left)
    g.assert(sym.kind == skVar, left, ceSymIsNotAVariable % sym.name.stringVal)
    g.genExpr(right)
    g.popToVar(sym)

  of nkMember:
    # assignment to field
    g.assert(left[0].kind == nkIdent, left[0], ceIdentExpected)

    let name = left[0].stringVal
    g.assert(g.objectType != nil, left, ceInvalidMember % name)
    g.assert(name in g.objectType.fields, left, ceFieldUndeclared % name)

    let id = g.objectType.fields[name]
    g.genExpr(right)
    g.chunk.emitOpcode(opcAssignToField)
    g.chunk.emitU8(id)

  of nkDot:
    # setter call
    let
      receiver = left[0]
      setter = left[1]
    g.assert(setter.kind == nkIdent, setter, ceIdentExpected)
    g.genExpr(receiver)
    g.genExpr(right)

    let
      name = setter.stringVal & '='
      vid = g.a.getVtableIndex(name, paramCount = 2)
    g.chunk.emitOpcode(opcCallMethod)
    g.chunk.emitU16(vid.uint16)

  else:
    g.error(left, ceAsgnInvalidLHS)

const specialInfixOps = ["=", "and", "or", "of"]

proc genSpecialInfix(g: CodeGen, n: Node) =
  ## Generates code for special infix operators (``=``, ``and``, ``or``).

  assert n.kind == nkInfix
  g.lineInfoFrom(n)  # redundant

  case n[0].stringVal
  of "=": g.genAssignment(n[1], n[2])
  of "and": unreachable "and is NYI"
  of "or": unreachable "or is NYI"
  of "of": unreachable "of is NYI"
  else: unreachable "operator must be one of the special infix operators"

proc genCall(g: CodeGen, n: Node) =
  ## Generates code for a procedure or method call.

  assert n.kind in {nkPrefix, nkInfix, nkCall}
  g.lineInfoFrom(n)

  # the only way of calling a proc is via ident() or (expr)()
  # anything else (operators, a.ident()) is a method call
  let isProcOrSelfCall = n.kind == nkCall and n[0].kind != nkDot

  if isProcOrSelfCall:

    case n[0].kind

    of nkIdent:
      let sym = g.lookupSymbol(n[0])
      case sym.kind
      of skProc:
        let p = g.a.procedures[sym.procId]
        g.assert(n.len - 1 == p.paramCount, n, ceWrongParamCount % [
          sym.name.stringVal,
          $p.paramCount, $(n.len - 1)
        ])
        for arg in n[1..^1]:
          g.genExpr(arg)
        g.chunk.emitOpcode(opcCallProc)
        g.chunk.emitU16(uint16 sym.procId)
      of skVar: unreachable "closure calls are NYI"
      else: g.error(n[0], ceSymCannotBeCalled % n[0].stringVal)

    of nkMember:
      g.assert(n[0][0].kind == nkIdent, n[0][0], ceIdentExpected)

      let name = n[0][0].stringVal
      g.assert(g.objectType != nil, n[0], ceInvalidMember % name)
      if name in g.objectType.fields:
        unreachable "closure calls (via member) are NYI"
      else:
        # self is always the 0th argument
        g.pushVar(g.self)

        # push the args
        for arg in n[1..^1]:
          g.genExpr(arg)

        # call the method
        let
          argc = n.len
          mid = g.a.getVtableIndex(name, argc)
        g.chunk.emitOpcode(opcCallMethod)
        g.chunk.emitU16(uint16 mid)

    else: unreachable "closure calls are NYI"

  else:

    let
      name =
        case n.kind
        of nkPrefix, nkInfix:
          n[0].stringVal
        of nkCall:
          # n[0] is always nkDot - see isProcCall declaration
          g.assert(n[0][1].kind == nkIdent, n[0][1], ceIdentExpected)
          n[0][1].stringVal
        else: "<unreachable>"
      argc =
        case n.kind
        of nkPrefix: 1
        of nkInfix: 2
        of nkCall: n.len
        else: -1

    if n.kind == nkInfix and name in specialInfixOps:
      g.genSpecialInfix(n)
      return

    if n.kind == nkCall:
      g.genExpr(n[0][0])

    for arg in n[1..^1]:
      g.genExpr(arg)

    let mid = g.a.getVtableIndex(name, argc)
    g.chunk.emitOpcode(opcCallMethod)
    g.chunk.emitU16(uint16 mid)

proc genDot(g: CodeGen, n: Node) =
  ## Generates code for a property access method call (a.b).

  assert n.kind == nkDot
  g.lineInfoFrom(n)

  g.assert(n[1].kind == nkIdent, n[1], ceIdentExpected)
  let mid = g.a.getVtableIndex(n[1].stringVal, 1)

  g.genExpr(n[0])
  g.chunk.emitOpcode(opcCallMethod)
  g.chunk.emitU16(mid.uint16)

proc genConstr(g: CodeGen, n: Node) =
  ## Generates code for an object constructor.

  assert n.kind == nkConstr
  g.lineInfoFrom(n)

  let sym = g.lookupSymbol(n[0])
  g.assert(sym.kind == skObject, n[0], ceSymIsNotAnObject)

  var fieldValues: seq[Node]
  fieldValues.setLen(sym.fields.len)
  for pair in n[1..^1]:
    let
      (nameNode, value) = (pair[0], pair[1])
      name = nameNode.stringVal
    g.assert(name in sym.fields, nameNode, ceFieldUndeclared % name)
    let index = sym.fields[name]
    fieldValues[index] = value

  var uninitialized: seq[string]
  for i, value in fieldValues:
    if uninitialized.len == 0 and value != nil:
      g.genExpr(value)
    else:
      uninitialized.add(sym.fieldNames[i])

  if uninitialized.len > 0:
    g.error(n, ceFieldsUninitialized % uninitialized.join(", "))

  g.chunk.emitOpcode(opcNewObject)
  g.chunk.emitU16(sym.vtable)
  g.chunk.emitU8(uint8 fieldValues.len)

proc genMember(g: CodeGen, n: Node) =
  ## Generates code for member access.

  g.assert(n[0].kind == nkIdent, n[0], ceIdentExpected)

  let name = n[0].stringVal
  g.assert(g.objectType != nil, n, ceInvalidMember % name)

  if name in g.objectType.fields:
    g.chunk.emitOpcode(opcPushField)
    g.chunk.emitU8(g.objectType.fields[name])
  else:
    g.error(n[0], ceMemberUndeclared % name)

proc genBlockExprOrStmt(g: CodeGen, n: Node) =
  ## Generates code for a block expression or statement.

  assert n.kind in {nkBlockExpr, nkBlockStmt}
  g.lineInfoFrom(n)

  g.withNewPseudoscope:
    var exprResult: Symbol
    if n.kind == nkBlockExpr:
      exprResult = g.defineVar(identNode(":blockExprResult"))
      g.chunk.emitOpcode(opcPushNil)
      g.popToVar(exprResult)
    g.withNewScope:
      g.genStmtList(n[0], isExpr = n.kind == nkBlockExpr, exprResult)

proc genIf(g: CodeGen, n: Node) =
  ## Generates code for an if expression or statement.

  assert n.kind in {nkIfExpr, nkIfStmt}
  g.lineInfoFrom(n)

  let isExpr = n.kind == nkIfExpr

  var
    afterIfBranches: seq[int]
    hadElse = false
    exprResult: Symbol

  g.withNewPseudoscope:
    if isExpr:
      exprResult = g.defineVar(identNode(":ifExprResult"))
      g.chunk.emitOpcode(opcPushNil)
      g.popToVar(exprResult)

    for branch in n:
      case branch.kind
      of nkIfBranch:
        # condition
        g.genExpr(branch[0])

        # jump past the body if the condition is falsey
        let afterBody = g.chunk.emitJump(opcJumpFwdIfFalsey)

        # discard condition and execute body
        # after the body's executed, make one more jump to the very end of
        # the statement
        g.chunk.emitOpcode(opcDiscard)
        g.chunk.emitU8(1)
        g.withNewScope:
          g.genStmtList(branch[1], isExpr, exprResult)
        afterIfBranches.add(g.chunk.emitJump(opcJumpFwd))

        # if the branch is falsey, we land here and discard the condition
        g.chunk.patchJump(afterBody)
        g.chunk.emitOpcode(opcDiscard)
        g.chunk.emitU8(1)

      of nkElseBranch:
        # else doesn't need to do any magic
        g.withNewScope:
          g.genStmtList(branch[0], isExpr, exprResult)
        hadElse = true

      else: unreachable

  if isExpr and not hadElse:
    g.error(n, ceIfExprMustHaveElse)

  for i in afterIfBranches:
    g.chunk.patchJump(i)

proc addMethodRecursively(a: Assembly, objectSym: Symbol,
                          name: string, paramCount: int, chunk: Chunk) =
  ## Helper for adding a method recursively to an object and its children.

  # right now this is a little bit inefficient because it has to look up the
  # method ID twice for each call to addMethodRecursively

  assert objectSym.kind == skObject

  # first, *always* add the method to the object itself
  let mid = a.getVtableIndex(name, paramCount)
  a.addMethod(int objectSym.vtable, name, paramCount, chunk)
  objectSym.methods.incl(mid.uint16)

  # then update children recursively if and only if they haven't implemented
  # the method themselves
  for childSym in objectSym.children:
    let vid = childSym.vtable
    if not a.vtables[vid].hasMethod(mid):
      addMethodRecursively(a, childSym, name, paramCount, chunk)

proc genProc(g: CodeGen, n: Node) =
  ## Generates code for a procedure declaration, method, or closure.

  assert n.kind in {nkProc, nkClosure}
  g.lineInfoFrom(n)

  let
    isNamed = n.kind == nkProc
    isClosure = g.scopes.len > 0 or not isNamed
    isMethod = g.objectType != nil
  assert not isClosure, "closures are NYI"

  # set up a new codegen
  var cg = g.createSub()
  cg.chunk = newChunk(g.chunk.filename)
  let
    (name, params, body) = (n[0], n[1], n[2])
    paramCount =
      if params.kind == nkEmpty: 0
      else: params.len

  # for methods, add them to the object's vtable
  if isMethod:
    addMethodRecursively(g.a, g.objectType, name.stringVal, paramCount + 1,
                         cg.chunk)

  # for top-level procedures, add them to the scope
  else:
    let
      sym = newSymbol(skProc, name)
      p = g.a.addProc(name.stringVal, paramCount, cg.chunk)
    sym.procId = p.id
    g.addSymbol(sym)

  # create a new scope. this scope is never popped as opcReturn makes sure that
  # all variables are removed from the stack
  cg.pushScope()

  # special self parameter (if applicable)
  if isMethod:
    cg.self = cg.defineVar(identNode("self").lineInfoFrom(params))
    cg.self.isSet = true

  # parameters
  if params.kind != nkEmpty:
    for name in params:
      var sym = cg.defineVar(name)
      sym.isSet = true

  # result variable
  cg.lineInfoFrom(params)
  cg.chunk.emitOpcode(opcPushNil)
  cg.result = cg.defineVar(identNode("result").lineInfoFrom(params))
  cg.result.isSet = true

  # body
  cg.withNewFlowBlock(fbkProcBody):
    cg.genStmtList(body, isExpr = false)

  # return the value stored in `result`
  # `result` is already at the top of the stack so we don't need to copy it
  # one more time
  cg.chunk.emitOpcode(opcReturn)

  # `self` is no more
  g.self = nil

proc genExpr(g: CodeGen, n: Node) =
  ## Generates code for an expression.

  g.lineInfoFrom(n)

  case n.kind
  of nkParen: g.genExpr(n[0])
  of nkNil..nkString: g.genLiteral(n)
  of nkIdent: g.genVarLookup(n)
  of nkPrefix, nkInfix, nkCall: g.genCall(n)
  of nkConstr: g.genConstr(n)
  of nkMember: g.genMember(n)
  of nkDot: g.genDot(n)
  of nkBlockExpr: g.genBlockExprOrStmt(n)
  of nkIfExpr: g.genIf(n)
  of nkClosure: g.genProc(n)
  else: g.error(n, ceExprExpected)

proc genVar(g: CodeGen, n: Node) =
  ## Generates code for a variable declaration.

  assert n.kind == nkVar
  g.lineInfoFrom(n)

  # note: multiple var names aren't supported by the syntax yet, but the logic
  # is here.
  for name in n[0]:
    g.genExpr(n[1])
    let sym = g.defineVar(name)
    g.popToVar(sym)

template genLoop(g: CodeGen, cond, body: untyped): untyped =
  ## Skeleton for loop generation. Used in ``genWhile`` and ``genFor`` to reduce
  ## repetition.

  block:
    g.withNewFlowBlock(fbkLoopOuter):
      # emit condition
      let loopStart = g.chunk.bytecode.len
      `cond`
      let jumpAfterLoop = g.chunk.emitJump(opcJumpFwdIfFalsey)

      # discard condition
      g.chunk.emitOpcode(opcDiscard)
      g.chunk.emitU8(1)

      # emit body
      g.withNewFlowBlock(fbkLoopIteration):
        `body`

      # jump back to start
      g.chunk.emitOpcode(opcJumpBack)
      g.chunk.emitU16(uint16 g.chunk.bytecode.len - loopStart + 2)

      g.chunk.patchJump(jumpAfterLoop)

      # remember to also discard the condition if the loop was jumped over
      g.chunk.emitOpcode(opcDiscard)
      g.chunk.emitU8(1)

proc genWhile(g: CodeGen, n: Node) =
  ## Generates code for a while loop.

  assert n.kind == nkWhile
  g.lineInfoFrom(n)

  g.genLoop do:
    # condition
    g.genExpr(n[0])
  do:
    # body
    g.genStmtList(n[1], isExpr = false)

proc genFor(g: CodeGen, n: Node) =
  ## Generates code for a for loop.

  # note for those hunting for performance:
  # it is faster to use a for loop than a while loop to perform protocol
  # iteration, because a for loop can make use of more efficient instructions.
  # namely, assigning to the loop variable can use the faster PopToVar
  # opcode, which avoids saving the old value of the variable.

  assert n.kind == nkFor
  g.lineInfoFrom(n)

  var
    iteratorSym: Symbol
    loopVarSym: Symbol

  let varList = n[0]
  assert varList.len == 1, "only one variable is supported"
  assert varList[0].kind == nkIdent

  g.withNewScope:

    # first, the iterator variable
    # a for loop calls the `_iterate` special method on the value being iterated
    let iter = n[1]
    g.genExpr(iter)
    g.chunk.emitOpcode(opcCallMethod)
    g.chunk.emitU16(uint16 g.a.special.iterate)
    iteratorSym = g.defineVar(identNode":iterator")
    assert iteratorSym.isLocalVar
    g.popToVar(iteratorSym)  # mark it as set

    # then, the loop variable
    # we create it outside of the loop body's scope to minimize stack operations
    g.chunk.emitOpcode(opcPushNil)
    loopVarSym = g.defineVar(varList[0])
    assert loopVarSym.isLocalVar
    g.popToVar(loopVarSym)  # also mark it as set


    g.genLoop do:
      # as the condition, we use :iterator._hasNext
      g.pushVar(iteratorSym)
      g.chunk.emitOpcode(opcCallMethod)
      g.chunk.emitU16(uint16 g.a.special.hasNext)
    do:
      # in the body, first, we need to update the loop var
      # we call :iterator._next to obtain the value, then we pop it to the var
      # note that we don't use assignment to reduce stack operations
      g.pushVar(iteratorSym)
      g.chunk.emitOpcode(opcCallMethod)
      g.chunk.emitU16(uint16 g.a.special.next)
      g.chunk.emitOpcode(opcPopToLocal)
      g.chunk.emitU16(loopVarSym.stackPos.uint16)

      # then we can execute the loop body
      let loop = n[2]
      g.genStmtList(loop, isExpr = false)

proc genBreak(g: CodeGen, n: Node) =
  ## Generates code for a break statement.

  assert n.kind == nkBreak
  g.lineInfoFrom(n)

  if not g.breakFlowBlock(fbkLoopOuter):
    g.error(n, ceInvalidBreak)

proc genContinue(g: CodeGen, n: Node) =
  ## Generates code for a continue statement.

  assert n.kind == nkContinue
  g.lineInfoFrom(n)

  if not g.breakFlowBlock(fbkLoopIteration):
    g.error(n, ceInvalidContinue)

proc genReturn(g: CodeGen, n: Node) =
  ## Generates code for a return statement.

  assert n.kind == nkReturn
  g.lineInfoFrom(n)

  g.assert(g.result != nil, n, ceInvalidReturn)

  if n[0].kind == nkEmpty:
    g.pushVar(g.result)
  else:
    g.genExpr(n[0])
  g.chunk.emitOpcode(opcReturn)

proc genObject(g: CodeGen, n: Node) =
  ## Generates code for an object definition.

  assert n.kind == nkObject
  g.lineInfoFrom(n)  # even though we don't really generate any code :p

  let (name, parentName, fields) = (n[0], n[1], n[2])
  var sym = newSymbol(skObject, name)
  sym.vtable = uint16 g.a.addVtable(name.stringVal)

  if parentName.kind != nkEmpty:
    var parent = g.lookupSymbol(parentName)
    g.assert(parent.kind == skObject, parentName,
             ceSymIsNotAnObject % parentName.stringVal)

    # save the new object as a child in the parent
    parent.children.add(sym)

    # inherit existing fields
    for name, index in parent.fields:
      sym.fields[name] = index
    sym.fieldNames.add(parent.fieldNames)

    # inherit existing methods
    let parentvt = g.a.vtables[parent.vtable]
    for mid in parent.methods:
      if parentvt.hasMethod(int mid):
        if g.a.vtables[sym.vtable].methods.len <= int mid:
          g.a.vtables[sym.vtable].methods.setLen(mid + 1)
        g.a.vtables[sym.vtable].methods[mid] = parentvt.methods[mid]

  for nameNode in fields:
    let name = nameNode.stringVal
    g.assert(name notin sym.fields, nameNode, ceFieldAlreadyExists % name)
    sym.fields[name] = uint8 sym.fields.len
    sym.fieldNames.add(name)

  g.addSymbol(sym)

proc genImpl(g: CodeGen, n: Node) =
  ## Generates code for an object implementation.

  let
    nameNode = n[0]
    sym = g.lookupSymbol(nameNode)
  g.assert(sym.kind == skObject, nameNode,
           ceSymIsNotAnObject % nameNode.stringVal)

  # these may not be nested so we may as well
  g.objectType = sym
  for def in n[1]:
    g.assert(def.kind == nkProc, def, ceImplInvalid)
    g.genProc(def)
  g.objectType = nil

proc genStmt(g: CodeGen, n: Node) =
  ## Generates code for a statement.

  g.lineInfoFrom(n)

  case n.kind
  of nkVar: g.genVar(n)
  of nkBlockStmt: g.genBlockExprOrStmt(n)
  of nkIfStmt: g.genIf(n)
  of nkWhile: g.genWhile(n)
  of nkFor: g.genFor(n)
  of nkBreak: g.genBreak(n)
  of nkContinue: g.genContinue(n)
  of nkProc: g.genProc(n)
  of nkReturn: g.genReturn(n)
  of nkObject: g.genObject(n)
  of nkImpl: g.genImpl(n)
  else:
    # expressions always have a result
    g.genExpr(n)
    g.chunk.emitOpcode(opcDiscard)
    g.chunk.emitU8(1)

proc genScript*(g: CodeGen, n: Node) =
  ## Generates code for a toplevel statement list.

  g.lineInfoFrom(n)
  g.genStmtList(n, isExpr = false)
  g.chunk.emitOpcode(opcHalt)
