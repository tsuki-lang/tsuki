import std/strutils

import ast
import common
import errors
import lexer


# general

template parseCommaSep(l: var Lexer, dest: var seq[Node], fin: static TokenKind,
                       rule: untyped) =
  ## Parses a comma-separated list of ``rule``.

  while true:
    case l.peek().kind
    of tkEof:
      l.error(peTokenMissing % $fin)
    of fin:
      discard l.next()
      break
    else: discard
    dest.add(`rule`)
    case l.next().kind
    of tkComma: continue
    of fin: break
    else:
      l.error(peXExpected % "',' or '" & $fin & "'")

proc parseExpr(l: var Lexer, precedence = -1): Node

proc parseStmt(l: var Lexer): Node

proc parseBlock(l: var Lexer, dest: var seq[Node],
                fin: static set[TokenKind]) =
  ## Parses a block that ends with ``fin``.

  while true:
    if l.atEnd:
      l.error(peTokenMissing % $fin)
    if l.peek().kind in fin:
      break
    dest.add(l.parseStmt())
    l.skip(tkSemi)

proc parseIf(l: var Lexer, token: Token, isStmt: bool): Node =
  ## Parses an if expression or statement.

  let astKind =
    if isStmt: nkIfStmt
    else: nkIfExpr
  result = astKind.tree().lineInfoFrom(token)

  var hasElse = false
  while true:
    var
      condition = l.parseExpr()
      stmts = nkStmtList.tree().lineInfoFrom(l.peek())
      branch = nkIfBranch.tree(condition, stmts).lineInfoFrom(condition)
    l.parseBlock(stmts.sons, {tkElif, tkElse, tkEnd})
    result.add(branch)

    let token = l.next()
    case token.kind
    of tkElif: continue
    of tkElse:
      hasElse = true
      break
    of tkEnd: return
    else: l.error(token, peXExpected % "'elif', 'else', or 'end'")

  if hasElse:
    var
      stmts = nkStmtList.tree().lineInfoFrom(l.peek())
      branch = nkElseBranch.tree(stmts).lineInfoFrom(stmts)
    l.parseBlock(stmts.sons, {tkEnd})
    result.add(branch)
    discard l.next()  # should always be 'end'

proc parseProc(l: var Lexer, token: Token, anonymous: static bool): Node =
  ## Parses a procedure or a closure.

  var name = emptyNode().lineInfoFrom(token)
  if not anonymous:
    let nameToken =
      l.expect({tkIdent, tkOperator}, peXExpected % "procedure name")
    var nameStr =
      case nameToken.kind
      of tkIdent: nameToken.ident
      of tkOperator: nameToken.operator
      else: "<invalid>"
    if l.peekOperator("="):
      discard l.next()
      nameStr.add('=')
    name = identNode(nameStr).lineInfoFrom(nameToken)

  var params = emptyNode().lineInfoFrom(name)
  if l.peek().kind == tkLParen:
    let paren = l.next()
    params = nkParamList.tree().lineInfoFrom(paren)
    l.parseCommaSep(params.sons, tkRParen):
      let nameToken = l.expect(tkIdent, peXExpected % "parameter name")
      identNode(nameToken)

  var body = nkStmtList.tree().lineInfoFrom(l.peek())
  if l.peekOperator("="):
    let eqToken = l.next()
    if l.peekOperator("..."):
      body = emptyNode().lineInfoFrom(l.next())
    else:
      let token = l.peek()
      body.add(nkReturn.tree(l.parseExpr()).lineInfoFrom(eqToken))
  else:
    l.parseBlock(body.sons, {tkEnd})
    discard l.next()  # always tkEnd

  let astKind =
    if anonymous: nkClosure
    else: nkProc
  result = astKind.tree(name, params, body).lineInfoFrom(token)

const
  pathPrecedence = 11

proc parsePrefix(l: var Lexer, token: Token): Node =
  ## Parses a prefix expression.

  case token.kind
  of tkNil: result = nilNode()
  of tkTrue: result = trueNode()
  of tkFalse: result = falseNode()
  of tkFloat: result = floatNode(token.floatVal)
  of tkString: result = stringNode(token.stringVal)
  of tkIdent: result = identNode(token.ident)
  of tkOperator:
    let
      op = identNode(token.operator).lineInfoFrom(token)
      expr =
        if token.operator == "@":
          l.parsePrefix(l.next())
        else:
          l.parseExpr(pathPrecedence - 1)
    result = nkPrefix.tree(op, expr).lineInfoFrom(op)
  of tkDot:
    let name = l.expect(tkIdent, peXExpected % "identifier")
    let member = identNode(name).lineInfoFrom(token)
    result = nkMember.tree(member)
  of tkLParen:
    result = nkParen.tree(l.parseExpr())
    discard l.expect(tkRParen, peTokenMissing % ")")
  of tkIf: result = l.parseIf(token, isStmt = false)
  of tkProc: result = l.parseProc(token, anonymous = true)
  else: l.error(token, peUnexpectedToken % $token)

  result.lineInfoFrom(token)

proc parseInfix(l: var Lexer, left: Node, token: Token): Node =
  ## Parses an infix expression.

  case token.kind
  of tkOperator:
    let op = identNode(token.operator).lineInfoFrom(token)
    result = nkInfix.tree(op, left, l.parseExpr(token.precedence))
  of tkDot:
    result = nkDot.tree(left, l.parseExpr(pathPrecedence))
  of tkLParen:
    result = nkCall.tree(left)
    l.parseCommaSep(result.sons, tkRParen):
      l.parseExpr()
  of tkLBrace:
    result = nkConstr.tree(left)
    l.parseCommaSep(result.sons, tkRBrace):
      let
        fieldName = l.expect(tkIdent, peXExpected % "field name")
        field = identNode(fieldName)
      l.expectOperator("=", peXExpected % "'='")
      nkFieldVal.tree(field, l.parseExpr()).lineInfoFrom(fieldName)
  else: l.error(token, peUnexpectedToken % $token)

  result.lineInfoFrom(token)

proc precedence(token: Token): int =
  ## Returns the precedence for the given token.

  case token.kind
  of tkOperator: token.precedence
  of tkLParen, tkLBrace, tkDot: pathPrecedence
  else: -10

proc parseExpr(l: var Lexer, precedence = -1): Node =
  ## Parses an expression.

  var token = l.next()

  result = l.parsePrefix(token)
  while precedence < precedence(l.peek()):
    token = l.next()
    if token.kind == tkEof:
      break
    result = l.parseInfix(result, token)

proc parseVar(l: var Lexer): Node =
  ## Parses a variable declaration.

  let
    varToken = l.next()  # always tkVar (see parseStmt)
    name = l.expect(tkIdent, peXExpected % "variable name")
    nameNode = identNode(name)
    names = nkVarList.tree(nameNode).lineInfoFrom(nameNode)
  l.expectOperator("=", peXExpected % "'='")
  let value = l.parseExpr()
  result = nkVar.tree(names, value).lineInfoFrom(varToken)

proc parseBlockStmt(l: var Lexer): Node =
  ## Parses a block statement.

  let blockToken = l.next()  # always tkBlock (see parseStmt)
  result = nkBlockStmt.tree().lineInfoFrom(blockToken)
  l.parseBlock(result.sons, {tkEnd})
  discard l.next()  # always tkEnd

proc parseWhile(l: var Lexer): Node =
  ## Parses a while loop.

  let
    whileToken = l.next()  # always tkWhile (see parseStmt)
    condition = l.parseExpr()
  l.skip(tkSemi)

  var loop = nkStmtList.tree().lineInfoFrom(l.peek())
  l.parseBlock(loop.sons, {tkEnd})
  discard l.next()  # always tkEnd

  result = nkWhile.tree(condition, loop).lineInfoFrom(whileToken)

proc parseFor(l: var Lexer): Node =
  ## Parses a for-in loop.

  let
    forToken = l.next()  # always tkFor (see parseStmt)
    forVarName = l.expect(tkIdent, peXExpected % "loop variable")
  discard l.expect(tkIn, peXExpected % "'in'")
  let iter = l.parseExpr()
  l.skip(tkSemi)

  var loop = nkStmtList.tree().lineInfoFrom(l.peek())
  l.parseBlock(loop.sons, {tkEnd})
  discard l.next()  # always tkEnd

  let
    forVar = identNode(forVarName)
    varList = nkVarList.tree(forVar).lineInfoFrom(forVar)
  result = nkFor.tree(varList, iter, loop).lineInfoFrom(forToken)

proc parseImpl(l: var Lexer): Node =
  ## Parses an object implementation block.

  let
    implToken = l.next()  # always tkImpl (see parseStmt)
    objectNameToken = l.expect(tkIdent, peXExpected % "object name")
    objectName = identNode(objectNameToken)

  var body = nkStmtList.tree().lineInfoFrom(implToken)
  l.parseBlock(body.sons, {tkEnd})
  discard l.next()  # always tkEnd

  result = nkImpl.tree(objectName, body).lineInfoFrom(implToken)

proc parseObject(l: var Lexer): Node =
  ## Parses an object definition.

  let
    objectToken = l.next()  # always tkObject (see parseStmt)
    nameToken = l.expect(tkIdent, peXExpected % "object name")
    name = identNode(nameToken)

  var parentName = emptyNode().lineInfoFrom(name)
  if l.peekOperator("of"):
    discard l.next()
    let parentNameToken = l.expect(tkIdent, peXExpected % "parent name")
    parentName = identNode(parentNameToken)

  l.expectOperator("=", peXExpected % "'='")

  var fields = nkFieldList.tree().lineInfoFrom(l.peek())
  while true:
    let fieldNameToken = l.expect(tkIdent, peXExpected % "field name")
    fields.add(identNode(fieldNameToken))
    if l.peek().kind == tkComma:
      discard l.next()
      continue
    else:
      break

  result = nkObject.tree(name, parentName, fields).lineInfoFrom(objectToken)

proc parseStmt(l: var Lexer): Node =
  ## Parses a statement.

  case l.peek().kind
  of tkVar: result = l.parseVar()
  of tkBlock: result = l.parseBlockStmt()
  of tkIf: result = l.parseIf(l.next(), isStmt = true)
  of tkWhile: result = l.parseWhile()
  of tkFor: result = l.parseFor()
  of tkProc: result = l.parseProc(l.next(), anonymous = false)
  of tkObject: result = l.parseObject()
  of tkImpl: result = l.parseImpl()
  else: result = l.parseExpr()

proc parseScript*(l: var Lexer): Node =
  ## Parses a script.

  result = nkStmtList.tree().lineInfoFrom(l.peek())

  while true:
    if l.peek().kind == tkEof:
      break
    result.sons.add(l.parseStmt())
    l.skip(tkSemi)


# tests

when isMainModule:

  var cs = new(CompilerState)
  discard cs.addFilename("invalid filename")
  let filenameId = cs.addFilename("test.tsu")

  template test(name, input: string, parse, check: untyped) =

    echo "\n--- ", name
    try:
      var l {.inject.} = initLexer(cs, filenameId, input)
      let n {.inject.} = `parse`
      `check`
      echo "** AST output:"
      echo n.treeRepr
    except ParseError as e:
      echo "!! Error: ", e.msg

  proc walk(n: Node, callback: proc (n: Node)) =

    callback(n)
    if not n.isNil and n.kind notin LeafNodes:
      for o in n.sons:
        walk(o, callback)

  proc verifyNotNil(n: Node) =

    if n.isNil:
      echo "!! found nil node in AST"

  proc verifyLineInfo(n: Node) =

    n.walk proc (n: Node) =
      if n.lineInfo == (0, 0) or n.filename != filenameId:
        echo "!! missing line info in AST:"
        echo n.treeRepr

  template test(name, input: string, parse: untyped) =

    test(name, input):
      `parse`
    do:
      n.verifyNotNil()
      n.verifyLineInfo()

  # expressions
  test("math", "a = 2 + 2 * 2", l.parseExpr())
  test("members", ".a.b.c + .b", l.parseExpr())
  test("non-sigil prefix", "-a.b.c", l.parseExpr())
  test("sigil prefix", "@a.b.c", l.parseExpr())
  test("call echo", "echo(awd)", l.parseExpr())
  test("call no args", "echo()", l.parseExpr())
  test("call >1 arg", "echo(1, 2, 3)", l.parseExpr())
  test("object constructor", "MyObj { a = 1, b = 2 }", l.parseExpr())
  test("if expressions", """
    if a
      _
    elif b
      _
    else
      _
    end
  """, l.parseExpr())

  # statements
  test("script", """
    var x = 1
    var y = 2
    x + y * w;  # semicolon is required here because the next line starts with (
    (a + 1) * 2
  """, l.parseScript())

  test("while", """
    while true
      _
    end
  """, l.parseScript())

  test("for", """
    for i in 1..10
      _
    end
  """, l.parseScript())

  test("procedures", """
    proc long(a, b, c)
      _
    end

    proc no_params
      _
    end

    proc short = _

    proc forward = ...

    var closure = proc
      _
    end
  """, l.parseScript())

  test("objects", """
    object Box = value
    object Vec2 = x, y
  """, l.parseScript())

  test("object impl", """

    object Vec2 = x, y

    impl Vec2
      proc x = .x
      proc y = .y
    end

  """, l.parseScript())
