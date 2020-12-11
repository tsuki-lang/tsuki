import ast

type
  SymbolKind* = enum
    skVar
    skProc

  Symbol* = ref object
    name*: Node

    case kind*: SymbolKind
    of skVar:
      case isLocalVar*: bool
      of true:
        stackPos*: int
      of false:
        globalId*: int
      isSet*: bool
    of skProc:
      procId*: int

proc newSymbol*(kind: SymbolKind, name: Node): Symbol =
  Symbol(name: name, kind: kind)

proc newVarSymbol*(name: Node, isLocal: bool): Symbol =
  Symbol(name: name, kind: skVar, isLocalVar: isLocal)
