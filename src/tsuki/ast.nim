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
    nkIfExpr        # if expression
    nkClosure       # closure (proc without a name)

    # statements
    nkVar           # variable declaration
    nkBlockStmt     # block statement
    nkIfStmt        # if statement
    nkWhile         # while loop
    nkFor           # for loop
    nkBreak         # break statement
    nkContinue      # continue statement
    nkProc          # procedure or method
    nkReturn        # return statement
    nkObject        # object definition
    nkImpl          # object implementation

    # building blocks
    nkStmtList
    nkFieldVal      # object construction ``field = value`` pair
    nkParamList     # procedure parameters
    nkVarList       # variable names (used in var and for)
    nkFieldList     # object field names
    nkIfBranch      # if-condition-block or elif-condition-block
    nkElseBranch    # else-block

  NodeObj* = object
    filename*: FilenameId
    lineInfo*: LineInfo

    case kind*: NodeKind
    of nkEmpty, nkNil, nkTrue, nkFalse: discard
    of nkFloat: floatVal*: float64
    of nkString, nkIdent: stringVal*: string
    else: sons*: seq[Node]

  Node* = ref NodeObj

  Index = int | BackwardsIndex
  SomeSlice = Slice[int] | HSlice[int, BackwardsIndex]

const
  LeafNodes* = {nkEmpty..nkIdent}
  ExprNodes* = {nkPrefix..nkClosure}
  StmtNodes* = {nkVar..nkImpl}

proc `[]`*(node: Node, index: Index): Node =
  ## Indexes the node.
  node.sons[index]

proc `[]`*(node: Node, slice: SomeSlice): seq[Node] =
  ## Slices the node.
  node.sons[slice]

proc add*(node, son: Node) =
  ## Adds a son to the node.
  node.sons.add(son)

iterator items*(node: Node): Node =
  ## Iterates over the node's sons.

  for n in node.sons:
    yield n

proc len*(node: Node): int =
  ## Returns the amount of sons the node has.
  node.sons.len

proc emptyNode*(): Node =
  ## Returns a new empty node.
  Node(kind: nkEmpty)

proc nilNode*(): Node =
  ## Returns a new ``nil`` literal node.
  Node(kind: nkNil)

proc trueNode*(): Node =
  ## Returns a new ``true`` literal node.
  Node(kind: nkTrue)

proc falseNode*(): Node =
  ## Returns a new ``false`` literal node.
  Node(kind: nkFalse)

proc floatNode*(value: float32): Node =
  ## Returns a new float literal node.
  Node(kind: nkFloat, floatVal: value)

proc stringNode*(value: string): Node =
  ## Returns a new string literal node.
  Node(kind: nkString, stringVal: value)

proc identNode*(ident: string): Node =
  ## Returns a new identifier node. Note that operators are flattened to
  ## identifiers during parsing.
  Node(kind: nkIdent, stringVal: ident)

proc tree*(kind: NodeKind, children: varargs[Node]): Node =
  ## Returns a new tree node with the given children.

  result = Node(kind: kind)
  result.sons.add(@children)

proc lineInfoFrom*(node: Node, token: Token): Node {.discardable.} =
  ## Inherits line info from the given token.

  node.filename = token.filename
  node.lineInfo = token.lineInfo
  result = node

proc lineInfoFrom*(node, other: Node): Node {.discardable.} =
  ## Inherits line info from the given node.

  node.filename = other.filename
  node.lineInfo = other.lineInfo
  result = node

proc identNode*(token: Token): Node =
  ## Creates an identifier node from a token.
  identNode(token.ident).lineInfoFrom(token)

proc error*(l: var Lexer, node: Node, message: string) =
  ## Raises a parsing error with tht given message, at the given node.
  error(l.cs, node.filename, node.lineInfo, message)

proc treeRepr*(node: Node): string =
  ## Returns a human-readable tree representation of the given AST.

  if node.isNil: return "!nil"

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
