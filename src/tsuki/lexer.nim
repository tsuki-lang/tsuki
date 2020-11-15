import std/options
import std/strutils
import std/tables

import common
import errors

type
  TokenKind* = enum
    tkEof = "<end of file>"

    # literals
    tkNil = "nil", tkTrue = "true", tkFalse = "false"
    tkFloat = "<float>"
    tkString = "<string>"
    tkIdent = "<ident>"
    tkOperator = "<operator>"

    # keywords
    tkVar = "var", tkLet = "let"
    tkIf = "if", tkThen = "then", tkElse = "else", tkElif = "elif"
    tkWhile = "while"
    tkFor = "for", tkIn = "in",
    tkBreak = "break", tkContinue = "continue"
    tkFunction = "proc", tkReturn = "return"
    tkDo = "do", tkEnd = "end"
    tkObject = "object", tkImpl = "impl"
    tkImport = "import"

    # special
    tkLParen = "(", tkRParen = ")"
    tkLBracket = "[", tkRBracket = "]"
    tkLBrace = "{", tkRBrace = "}"
    tkComma = ",", tkSemi = ";"
    tkDot = "."

  Token* = object
    filename*: FilenameId
    lineInfo*: LineInfo

    case kind*: TokenKind
    of tkFloat:
      floatVal*: float64
    of tkString:
      stringVal*: string
    of tkIdent:
      ident*: string
    of tkOperator:
      operator*: string
      precedence*: int
    else: discard

  LineInfo* = tuple
    line, column: int

  PositionInfo* = tuple
    position: int
    lineInfo: LineInfo

  Lexer* = object
    cs: CompilerState

    input: string
    position: int
    peekCache: Option[(Token, PositionInfo)]

    filename: FilenameId
    lineInfo: LineInfo
    storedLineInfo: LineInfo

  ParseError* = object of ValueError
    lineInfo*: LineInfo

  ParseErrorRef* = ref ParseError

const
  liStartOfFile = LineInfo (1, 0)

  lineBreaks = {'\n', '\r'}
  whitespace = {' ', '\t'}
  eofChar = '\0'
  utf8 = {'\x80'..'\xFF'}
  allChars = {low(char)..high(char)}

  decDigits = {'0'..'9'}
  identStartChars = {'a'..'z', 'A'..'Z', '_'} + utf8
  identChars = identStartChars + decDigits
  # operators follow nim's rules
  operatorChars = {'=', '+', '-', '*', '/', '<', '>',
                   '@', '$', '~', '&', '%', '|',
                   '!', '?', '^', '.', ':', '\\'}

  tkKeywords = {tkNil, tkTrue, tkFalse, tkVar..tkImport}
  keywords = block:
    var pairs: seq[(string, TokenKind)]
    for kind in tkKeywords:
      pairs.add(($kind, kind))
    pairs.toTable
  keywordOperators* = {
    "mod": 9, "div": 9,
    "of": 5,
    "and": 4,
    "or": 3,
    "not": -1,  # prefix-only
  }.toTable

  firstCharPrecedence = block:
    let prec = {
      '$': 10, '^': 10,
      '*': 9, '%': 9, '\\': 9, '/': 9,
      '+': 8, '-': 8, '~': 8, '|': 8,
      '&': 7,
      '.': 6,
      '=': 5, '<': 5, '>': 5, '!': 5,
      '@': 2, ':': 2, '?': 2,
    }
    var a: array[char, int]
    for p in mitems(a): p = -1
    for (c, p) in prec:
      a[c] = p
    a

# tokens

{.push inline.}

proc getPrecedence(operator: string): int =

  assert operator.len > 0
  # standard operators
  result = firstCharPrecedence[operator[0]]
  if result == -1 and operator in keywordOperators:
    # special keyword operators
    result = keywordOperators[operator]
  elif operator[^1] == '=':
    # assignment-like operator
    if operator.len >= 2 and operator[0] in {'<', '>', '!', '=', '~', '?'}:
      return
    result = 1
  elif operator.len >= 2 and
       operator[^1] == '>' and operator[^2] in {'-', '~', '='}:
    # arrow-like operator
    result = 0


proc simpleToken(kind: TokenKind): Token = Token(kind: kind)

proc floatToken(value: float64): Token = Token(kind: tkFloat, floatVal: value)

proc stringToken(value: string): Token = Token(kind: tkString, stringVal: value)

proc identToken(ident: string): Token = Token(kind: tkIdent, ident: ident)

proc operatorToken(operator: string): Token =
  Token(
    kind: tkOperator,
    operator: operator,
    precedence: getPrecedence(operator)
  )

{.pop.}

proc repr*(t: Token): string =
  ## Debug representation for tokens.

  result.add("($#, $#) " % [$t.lineInfo.line, $t.lineInfo.column])
  result.add($t.kind)

  case t.kind
  of tkFloat: result.add(' '); result.addFloat(t.floatVal)
  of tkString: result.add(' '); result.addQuoted(t.stringVal)
  of tkIdent: result.add(' '); result.add(t.ident)
  of tkOperator:
    result.add(' ')
    result.add(t.operator)
    result.add(" ($#)" % $t.precedence)
  else: discard

proc `$`*(t: Token): string =

  case t.kind
  of tkFloat: result.addFloat(t.floatVal)
  of tkString: result.addQuoted(t.stringVal)
  of tkIdent: result.add(t.ident)
  of tkOperator: result.add(t.operator)
  else: result = $t.kind

# lexer

{.push inline.}

proc hasMore*(l: Lexer): bool =
  l.position < l.input.len

proc atEnd*(l: Lexer): bool =
  not l.hasMore

proc get(l: Lexer): char =
  if l.hasMore: l.input[l.position]
  else: eofChar

proc advance(l: var Lexer) =
  inc l.position
  inc l.lineInfo.column

proc savePosition*(l: Lexer): PositionInfo =
  (l.position, l.lineInfo)

proc restorePosition*(l: var Lexer, pi: PositionInfo) =
  (l.position, l.lineInfo) = pi

proc syncPosition(l: var Lexer) =
  l.storedLineInfo = l.lineInfo

proc error*(cs: CompilerState, filename: FilenameId, lineInfo: LineInfo,
            message: string) =
  raise ParseErrorRef(
    msg: errorFormat % [
      cs.getFilename(filename),
      $lineInfo.line, $lineInfo.column,
      message
    ],
    lineInfo: lineInfo,
  )

proc error*(l: var Lexer, message: string) =
  error(l.cs, l.filename, l.storedLineInfo, message)

proc error*(l: var Lexer, token: Token, message: string) =
  error(l.cs, token.filename, token.lineInfo, message)

proc readChars(l: var Lexer, set: set[char], dest: var string) =
  while l.get in set:
    dest.add(l.get)
    l.advance()

proc readString(l: var Lexer, quote: char, dest: var string) =
  l.advance()  # assume that the first char is valid
  while true:
    if not l.hasMore: l.error(leUnexpectedEof)
    if l.get == quote:
      l.advance()
      break
    dest.add(l.get)
    l.advance()

proc discardChars(l: var Lexer, set: set[char]) =
  while l.get in set:
    l.advance()

proc matchChar(l: var Lexer, set: set[char], dest: var string): bool =
  result = l.get in set
  if result:
    dest.add(l.get)
    l.advance()

{.pop.}


proc skipIgnored*(l: var Lexer) =
  ## Skips comments, whitespace, and line breaks.

  while true:
    case l.get
    of whitespace:
      l.discardChars(whitespace)
    of '\n':
      l.advance()
      inc l.lineInfo.line
      l.lineInfo.column = 0
    of '\r':
      l.advance()
      l.lineInfo.column = 0
    of '#':
      l.discardChars(allChars - lineBreaks)
    else: break

proc next*(l: var Lexer): Token =
  ## Returns the next token in the input string.

  # token caching for peek()
  if l.peekCache.isSome:
    var pi: PositionInfo
    (result, pi) = l.peekCache.get
    l.peekCache = none((Token, PositionInfo))
    (l.position, l.lineInfo) = pi
    return

  l.skipIgnored()
  l.syncPosition()

  case l.get
  of eofChar: result = simpleToken(tkEof)

  of '(': l.advance(); result = simpleToken(tkLParen)
  of ')': l.advance(); result = simpleToken(tkRParen)
  of '[': l.advance(); result = simpleToken(tkLBracket)
  of ']': l.advance(); result = simpleToken(tkRBracket)
  of '{': l.advance(); result = simpleToken(tkLBrace)
  of '}': l.advance(); result = simpleToken(tkRBrace)
  of ',': l.advance(); result = simpleToken(tkComma)
  of ';': l.advance(); result = simpleToken(tkSemi)

  of decDigits:
    var number = ""
    # whole part
    l.readChars(decDigits, number)
    # fractional part
    if l.matchChar({'.'}, number):
      l.readChars(decDigits, number)
    # exponent
    if l.matchChar({'e', 'E'}, number):
      discard l.matchChar({'-', '+'}, number)
      l.readChars(decDigits, number)
    result = floatToken(parseFloat(number))

  of '"':
    var str = ""
    l.readString('"', str)
    result = stringToken(str)

  of identStartChars:
    var ident = ""
    l.readChars(identChars, ident)
    if ident in keywords:
      result = simpleToken(keywords[ident])
    elif ident in keywordOperators:
      result = operatorToken(ident)
    else:
      result = identToken(ident)

  of operatorChars:
    var operator = ""
    l.readChars(operatorChars, operator)
    if operator == ".":
      result = simpleToken(tkDot)
    else:
      result = operatorToken(operator)

  elif not l.hasMore:
    l.error(leUnexpectedEof % $l.get)
  else: l.error(leUnexpectedChar % $l.get)

  result.filename = l.filename
  result.lineInfo = l.storedLineInfo

proc peek*(l: var Lexer): Token =
  ## Peeks a token from the stream.

  let pi = l.savePosition()
  result = l.next()
  l.peekCache = some((result, l.savePosition()))
  l.restorePosition(pi)

proc expect*(l: var Lexer, kind: TokenKind, error: string): Token =
  result = l.next()
  if result.kind != kind:
    l.error(result, error)

proc peekOperator*(l: var Lexer, op: string): bool =
  let token = l.next()
  result = token.kind == tkOperator and token.operator == op

proc expectOperator*(l: var Lexer, op, error: string) =
  let token = l.next()
  if token.kind != tkOperator or token.operator != op:
    l.error(token, error)

proc skip*(l: var Lexer, optional: TokenKind) =
  if l.peek().kind == optional:
    discard l.next()

proc initLexer*(cs: CompilerState, filename: FilenameId,
                input: string): Lexer {.inline.} =
  ## Creates and initializes a new lexer.
  Lexer(
    cs: cs,
    input: input,
    filename: filename,
    lineInfo: liStartOfFile,
  )

when isMainModule:
  const input = """
    # lexer test
    # this is a comment

    # literals
    nil true false
    123 3.14159265 6.67e-11
    "this is a string, hello world"
    snake_case camelCase PascalCase I_AM_SCREAMING_CASE

    # operators
    ^                   # 10
    * / div mod %       # 9
    + -                 # 8
    &                   # 7
    .. ..<              # 6
    == <= < >= > != of  # 5
    and                 # 4
    or                  # 3
    @ : ?               # 2
    = += -= *= /=       # 1
    -> ~> =>            # 0

    # keywords
    var let
    if then else elif
    while
    for in
    break continue
    proc return
    do end
    object impl
    import

    # special
    () [] {}
    , ;
    .
  """

  var
    cs = new(CompilerState)
    filenameId = cs.addFilename("test.tsu")
    lexer = initLexer(cs, filenameId, input)

  try:
    while true:
      let token = lexer.next()
      echo token.repr
      if token.kind == tkEof:
        break
  except ParseError as e:
    echo e.msg
    quit(1)
