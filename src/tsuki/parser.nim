import std/strutils

import ast
import common
import errors
import lexer

template parseCommaSep(l: var Lexer, dest: var seq[Node], fin: static TokenKind,
                       rule: untyped) =

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

proc parseExpr*(l: var Lexer, precedence = 0): Node

proc parsePrefix(l: var Lexer, token: Token): Node =

  case token.kind
  of tkNil: result = nilNode()
  of tkTrue: result = trueNode()
  of tkFalse: result = falseNode()
  of tkFloat: result = floatNode(token.floatVal)
  of tkString: result = stringNode(token.stringVal)
  of tkIdent: result = identNode(token.ident)
  of tkOperator:
    let op = identNode(token.operator).lineInfoFrom(token)
    result = nkPrefix.tree(op, l.parsePrefix(l.next()))
  of tkDot:
    let name = l.expect(tkIdent, peXExpected % "identifier")
    let member = identNode(name.ident).lineInfoFrom(token)
    result = nkMember.tree(member)
  of tkLParen:
    result = nkParen.tree(l.parseExpr())
    discard l.expect(tkRParen, peTokenMissing % ")")
  else: l.error(token, peUnexpectedToken % $token)

  result.lineInfoFrom(token)

const
  pathPrecedence = 11

proc parseInfix(l: var Lexer, left: Node, token: Token): Node =

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
        field = identNode(fieldName.ident).lineInfoFrom(fieldName)
      l.expectOperator("=", peXExpected % "'='")
      nkFieldVal.tree(field, l.parseExpr()).lineInfoFrom(fieldName)
  else: l.error(token, peUnexpectedToken % $token)

  result.lineInfoFrom(token)

proc precedence(token: Token): int =

  case token.kind
  of tkOperator: token.precedence
  of tkLParen, tkLBrace, tkDot: pathPrecedence
  else: -1

proc parseExpr(l: var Lexer, precedence = 0): Node =

  var token = l.next()

  result = l.parsePrefix(token)
  while precedence < precedence(l.peek()):
    token = l.next()
    if token.kind == tkEof:
      break
    result = l.parseInfix(result, token)

proc parseStmt(l: var Lexer): Node

proc parseBlock(l: var Lexer, dest: var seq[Node], fin: static TokenKind) =

  while true:
    if l.atEnd:
      l.error(peTokenMissing % $fin)
    dest.add(l.parseStmt())
    l.skip(tkSemi)
    if l.peek() == fin:
      break

proc parseStmt(l: var Lexer): Node =

  case l.peek().kind
  else:
    result = l.parseExpr()

proc parseScript*(l: var Lexer): Node =

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
  test("call echo", "echo(awd)", l.parseExpr())
  test("call no args", "echo()", l.parseExpr())
  test("call >1 arg", "echo(1, 2, 3)", l.parseExpr())
  test("object constructor", "MyObj { a = 1, b = 2 }", l.parseExpr())

  # statements
  test("script", """
    x = 1
    y = 2
    x + y * w;  # semicolon is required here because the next line starts with (
    (a + 1) * 2
  """, l.parseScript())
