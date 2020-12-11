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
  Scope* = object
    syms: Table[string, Symbol]
    vars: int

  CodeGen* = ref object
    cs: CompilerState
    a: Assembly
    module: Module
    chunk: Chunk

    scopes: seq[Scope]

  CompileError* = object of ValueError
    filename*: FilenameId
    lineInfo*: LineInfo

  CompileErrorRef = ref CompileError


# init

proc initCodeGen*(cs: CompilerState, assembly: Assembly,
                  module: Module, chunk: Chunk): CodeGen =
  ## Creates and initializes a new codegen instance.

  CodeGen(
    cs: cs, a: assembly,
    module: module, chunk: chunk
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

proc pushScope(g: CodeGen) =
  ## Pushes a new scope.
  g.scopes.add(Scope())

proc popScope(g: CodeGen) =
  ## Pops the current scope, together with its variables.
  let scope = g.scopes.pop()
  g.chunk.emitOpcode(opcDiscard)
  g.chunk.emitU8(scope.vars.uint8)

template withNewScope(g: CodeGen, body: untyped) =
  ## Helper for automatically pushing and popping a scope.

  g.pushScope()
  `body`
  g.popScope()

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

proc defineVar(g: CodeGen, name: Node): Symbol =
  ## Defines a new variable with the given name and returns its symbol.
  ## To set the value, use ``popToVar``.

  # local variable
  if g.scopes.len > 0:
    result = newVarSymbol(name, isLocal = true)
    result.stackPos = g.scope.vars
    inc g.scope.vars
    g.scope.syms[name.stringVal] = result

  # global variable
  else:
    result = newVarSymbol(name, isLocal = false)
    result.globalId = g.a.globalVarCount
    inc g.a.globalVarCount
    g.module.globalSyms[name.stringVal] = result

  assert result != nil

proc pushVar(g: CodeGen, sym: Symbol) =
  ## Pushes a copy of the variable's value onto the stack.

  assert sym.kind == skVar
  if sym.isLocalVar:
    g.chunk.emitOpcode(opcPushLocal)
    g.chunk.emitU16(sym.stackPos.uint16)
  else:
    g.chunk.emitOpcode(opcPushGlobal)
    g.chunk.emitU16(sym.globalId.uint16)

proc popToVar(g: CodeGen, sym: Symbol) =
  ## Pops the value at the top of the stack to the variable represented by the
  ## given symbol.

  assert sym.kind == skVar
  if sym.isLocalVar and sym.isSet:
    g.chunk.emitOpcode(opcPopToLocal)
    g.chunk.emitU16(sym.stackPos.uint16)
  elif not sym.isLocalVar:
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

proc genStmtList(g: CodeGen, n: Node) =
  ## Generates code for a statement list.

  assert n.kind == nkStmtList
  g.lineInfoFrom(n)

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
      vid = g.a.getVtableIndex(name, paramCount = 1)
    g.chunk.emitOpcode(opcCallMethod)
    g.chunk.emitU16(vid.uint16)
  else:
    g.error(left, ceAsgnInvalidLHS)

const specialInfixOps = ["=", "and", "or"]
proc genSpecialInfix(g: CodeGen, n: Node) =
  ## Generates code for special infix operators (``=``, ``and``, ``or``).

  assert n.kind == nkInfix
  g.lineInfoFrom(n)  # redundant

  case n[0].stringVal
  of "=": g.genAssignment(n[1], n[2])
  of "and": unreachable "and is NYI"
  of "or": unreachable "or is NYI"
  else: unreachable "operator must be one of the special infix operators"

proc genCall(g: CodeGen, n: Node) =
  ## Generates code for a procedure or method call.

  assert n.kind in {nkPrefix, nkInfix, nkCall}
  g.lineInfoFrom(n)

  # the only way of calling a proc is via ident() or (expr)()
  # anything else (operators, a.ident()) is a method call
  let isProcCall = n.kind == nkCall and n[0].kind != nkDot

  if isProcCall:

    if n[0].kind == nkIdent:
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
    else: unreachable "closure calls are NYI"

  else:

    let name =
      case n.kind
      of nkPrefix, nkInfix:
        n[0].stringVal
      of nkCall:
        # n[0] is always nkDot - see isProcCall declaration
        g.assert(n[0][1].kind == nkIdent, n[0][1], ceIdentExpected)
        n[0][1].stringVal
      else: "<unreachable>"

    if n.kind == nkInfix and name in specialInfixOps:
      g.genSpecialInfix(n)
      return

    for arg in n[1..^1]:
      g.genExpr(arg)

    let
      argc = n.len - 2
      vid = g.a.getVtableIndex(name, argc)

    g.chunk.emitOpcode(opcCallMethod)
    g.chunk.emitU16(vid.uint16)

proc genDot(g: CodeGen, n: Node) =
  ## Generates code for a property access method call (a.b).

  assert n.kind == nkDot
  g.lineInfoFrom(n)

  g.assert(n[1].kind == nkIdent, n[1], ceIdentExpected)
  let vid = g.a.getVtableIndex(n[1].stringVal, 0)

  g.genExpr(n[0])
  g.chunk.emitOpcode(opcCallMethod)
  g.chunk.emitU16(vid.uint16)
  g.chunk.emitU8(1)

proc genIf(g: CodeGen, n: Node) =
  ## Generates code for an if expression or statement.

  assert n.kind in {nkIfExpr, nkIfStmt}
  g.lineInfoFrom(n)

  let isExpr = n.kind == nkIfExpr
  assert not isExpr, "if expressions are NYI"

  var
    afterIfBranches: seq[int]
    hadElse = false

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
        g.genStmtList(branch[1])
      afterIfBranches.add(g.chunk.emitJump(opcJumpFwd))

      # if the branch is falsey, we land here and discard the condition
      g.chunk.patchJump(afterBody)
      g.chunk.emitOpcode(opcDiscard)
      g.chunk.emitU8(1)

    of nkElseBranch:
      # else doesn't need to do any magic
      g.withNewScope:
        g.genStmtList(branch[0])

    else: unreachable

  for i in afterIfBranches:
    g.chunk.patchJump(i)

proc genExpr(g: CodeGen, n: Node) =
  ## Generates code for an expression.

  g.lineInfoFrom(n)

  case n.kind
  of nkParen: g.genExpr(n)
  of nkNil..nkString: g.genLiteral(n)
  of nkIdent: g.genVarLookup(n)
  of nkPrefix, nkInfix, nkCall: g.genCall(n)
  of nkConstr: unreachable "objects are NYI"
  of nkMember: unreachable "objects are NYI"
  of nkDot: g.genDot(n)
  of nkIfExpr: g.genIf(n)
  of nkProc: unreachable "closures are NYI"
  else: unreachable "node must represent an expression"

proc genVar(g: CodeGen, n: Node) =
  ## Generates code for a variable declaration.

  assert n.kind == nkVar
  g.lineInfoFrom(n)

  for name in n[0]:
    g.genExpr(n[1])
    let sym = g.defineVar(name)
    g.popToVar(sym)

proc genBlockStmt(g: CodeGen, n: Node) =
  ## Generates code for a block statement.

  assert n.kind == nkBlockStmt
  g.lineInfoFrom(n)

  g.withNewScope:
    for stmt in n:
      g.genStmt(stmt)

proc genWhile(g: CodeGen, n: Node) =
  ## Generates code for a while loop.

  assert n.kind == nkWhile
  g.lineInfoFrom(n)

  # condition
  let loopStart = g.chunk.bytecode.len
  g.genExpr(n[0])
  let afterLoop = g.chunk.emitJump(opcJumpFwdIfFalsey)

  # body
  g.chunk.emitOpcode(opcDiscard)
  g.chunk.emitU8(1)
  g.withNewScope:
    g.genStmtList(n[1])

  # jump back to start
  g.chunk.emitOpcode(opcJumpBack)
  g.chunk.emitU16(uint16 g.chunk.bytecode.len - loopStart + 2)

  g.chunk.patchJump(afterLoop)
  g.chunk.emitOpcode(opcDiscard)
  g.chunk.emitU8(1)

proc genStmt(g: CodeGen, n: Node) =
  ## Generates code for a statement.

  g.lineInfoFrom(n)

  case n.kind
  of nkVar: g.genVar(n)
  of nkBlockStmt: g.genBlockStmt(n)
  of nkIfStmt: g.genIf(n)
  of nkWhile: g.genWhile(n)
  of nkFor: unreachable "for is NYI"
  of nkBreak: unreachable "control flow is NYI"
  of nkContinue: unreachable "control flow is NYI"
  of nkReturn: unreachable "procedures are NYI"
  of nkObject: unreachable "objects are NYI"
  of nkImpl: unreachable "objects are NYI"
  else:
    # expressions always have a result
    g.genExpr(n)
    g.chunk.emitOpcode(opcDiscard)
    g.chunk.emitU8(1)

proc genScript*(g: CodeGen, n: Node) =
  ## Generates code for a toplevel statement list.

  g.lineInfoFrom(n)
  g.genStmtList(n)
  g.chunk.emitOpcode(opcHalt)
