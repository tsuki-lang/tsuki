import std/strutils

import common
import lexer

type
  NodeKind* = enum
    nkEmpty

    # literals
    nkNil
    nkTrue, nkFalse
    nkFloat
    nkString
    nkIdent

    # expressions
    nkPrefix
    nkInfix
    nkParen         # parenthesized expression
    nkCall          # proc call
    nkConstr        # object construction
    nkMember        # member access ``.a``
    nkDot           # dot expression ``a.b``

    # building blocks
    nkStmtList
    nkFieldVal

  NodeObj* = object
    filename*: FilenameId
    lineInfo*: LineInfo

    case kind*: NodeKind
    of nkEmpty, nkNil, nkTrue, nkFalse: discard
    of nkFloat: floatVal*: float64
    of nkString, nkIdent: stringVal*: string
    else: sons*: seq[Node]

  Node* = ref NodeObj

  Index = int | BackwardsIndex | Slice[int] | HSlice[int, BackwardsIndex]

const
  LeafNodes* = {nkEmpty..nkIdent}

proc `[]`*(node: Node, index: Index): Node =
  node.sons[index]

iterator items*(node: Node): Node =
  for n in node.sons:
    yield n

proc emptyNode*(): Node = Node(kind: nkEmpty)

proc nilNode*(): Node = Node(kind: nkNil)

proc trueNode*(): Node = Node(kind: nkTrue)

proc falseNode*(): Node = Node(kind: nkFalse)

proc floatNode*(value: float32): Node = Node(kind: nkFloat, floatVal: value)

proc stringNode*(value: string): Node = Node(kind: nkString, stringVal: value)

proc identNode*(ident: string): Node = Node(kind: nkIdent, stringVal: ident)

proc tree*(kind: NodeKind, children: varargs[Node]): Node =

  result = Node(kind: kind)
  result.sons.add(@children)

proc lineInfoFrom*(node: Node, token: Token): Node {.discardable.} =

  node.filename = token.filename
  node.lineInfo = token.lineInfo
  result = node

proc treeRepr*(node: Node): string =

  case node.kind
  of nkEmpty: result = "Empty"
  of nkNil: result = "Nil"
  of nkTrue: result = "True"
  of nkFalse: result = "False"
  of nkFloat: result = "Float " & $node.floatVal
  of nkString: result = "String " & escape(node.stringVal)
  of nkIdent: result = "Ident " & node.stringVal
  else:
    result = ($node.kind)[2..^1]
    var children = ""
    for i, child in node.sons:
      children.add('\n' & child.treeRepr)
    result.add(children.indent(2))
