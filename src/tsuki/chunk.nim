import std/bitops
import std/options
import std/tables
import std/with

import ast
import common
import lexer
import symbol
import value

type
  Opcode* = enum

    # literals
    opcPushNil
    opcPushTrue
    opcPushFalse
    opcPushFloat
    opcPushString

    # global variables
    opcPushGlobal
    opcPopToGlobal
    opcAssignToGlobal

    # local variables
    opcPushLocal
    opcPopToLocal
    opcAssignToLocal

    # other stack ops
    opcDiscard

    # jumps
    opcJumpFwd
    opcJumpFwdIfFalsey
    opcJumpBack

    # calls
    opcCallProc
    opcCallMethod
    opcReturn

    # halting
    opcHalt

  JumpOpcode* = range[opcJumpFwd..opcJumpFwdIfFalsey]

  CodedLineInfo = tuple
    lineInfo: LineInfo
    runLength: int

  Chunk* = ref object
    filename*: FilenameId
    bytecode*: seq[uint8]
    currentLineInfo: LineInfo
    codedLineInfos: seq[CodedLineInfo]  # run length-encoded

  ProcedureKind* = enum
    pkBytecode  # tsuki proc
    pkNative    # nim proc

  BaseState* = ref object of RootObj
    ## Root object for VM execution. Used to break up dependency cycles.

  RawProcedureImpl* =
    proc (s: BaseState, args: openArray[Value]): Value {.closure.}
    ## This is a "raw" version for native proc implementations. The first
    ## parameter is **always** a vm.State, but this is used to break dependency
    ## cycles.
    ## This is a ref object because Nim codegen doesn't like casting closures,
    ## but it's fine with casting refs.

  Procedure* = object
    id*: int
    name*: string
    paramCount*: int
    case kind*: ProcedureKind
    of pkBytecode:
      chunk*: Chunk
    of pkNative:
      impl*: RawProcedureImpl

  Vtable* = object
    # TODO: this is less than efficient - benchmark the performance of using
    # a seq[Option[Procedure]] vs a Table[int, Procedure]
    name*: string
    methods: seq[Option[Procedure]]

  MethodId* = distinct int

  MethodSignature* = tuple
    name: string
    paramCount: int

  SpecialMethods* = object
    iterate*: int
    hasNext*: int
    next*: int

  Assembly* = ref object
    procedures*: seq[Procedure]
    vtables*: seq[Vtable]

    globalVarCount*: int
    methodIds: Table[MethodSignature, int]
    methodSignatures: seq[MethodSignature]

    special*: SpecialMethods

  Module* = ref object
    mainChunk*: Chunk
    globalSyms*: Table[string, Symbol]


proc `$`*(ms: MethodSignature): string =
  ## Stringifies a method signature.

  result.add(ms.name)
  result.add('(')
  for i in 1..ms.paramCount:
    result.add('_')
    if i < ms.paramCount:
      result.add(", ")
  result.add(')')

# vtable

proc hasMethod*(vt: Vtable, i: int): bool =
  ## Returns whether the vtable has the given method.
  i in 0..<vt.methods.len and vt.methods[i].isSome

proc getMethod*(vt: Vtable, i: int): lent Procedure =
  ## Gets the method with the index ``i`` from the given vtable.
  vt.methods[i].get

proc addMethod*(vt: var Vtable, i: int, name: string, paramCount: int,
                chunk: Chunk) =
  ## Adds a bytecode method to the vtable.

  if i notin 0..<vt.methods.len:
    vt.methods.setLen(i + 1)

  var p = Procedure(
    id: i,
    name: name, paramCount: paramCount + 1,  # add one for the receiver
    kind: pkBytecode, chunk: chunk,
  )
  vt.methods[i] = some p

proc addMethod*(vt: var Vtable, i: int, name: string, paramCount: int,
                impl: RawProcedureImpl) =
  ## Adds a native method to the vtable.

  if i notin 0..<vt.methods.len:
    vt.methods.setLen(i + 1)

  var p = Procedure(
    id: i,
    name: name, paramCount: paramCount + 1,  # again, add one for the receiver
    kind: pkNative, impl: impl,
  )
  vt.methods[i] = some p


# assembly


proc getVtableIndex*(a: Assembly, name: string, paramCount: int): int =
  ## Returns the vtable index of the method with the given name.
  ## This is type-unsafe and only for internal use, use ``getMethodId``
  ## instead.

  let sig = (name, paramCount)
  if sig notin a.methodIds:
    result = a.methodIds.len
    a.methodIds[sig] = result
    a.methodSignatures.add(sig)
  else:
    result = a.methodIds[sig]

proc newAssembly*(): Assembly =
  ## Creates and initializes a new assembly.

  new result
  result.vtables.setLen(vtableFirstObject)

  const vtableNames = [
    vtableNil: "Nil",
    vtableBool: "Bool",
    vtableFloat: "Float",
    vtableString: "String",
  ]
  for i, name in vtableNames:
    result.vtables[i].name = name

  with result.special:
    iterate = result.getVtableIndex("_iterate", 0)
    hasNext = result.getVtableIndex("_hasNext", 0)
    next = result.getVtableIndex("_next", 0)

{.push inline.}

proc getMethodId*(a: Assembly, name: string, paramCount: int): MethodId =
  ## Type-safe version of ``getVtableIndex``.
  ##
  ## Returns what other languages may call a "call handle", that is, a number
  ## uniquely identifying a specific method signature.

  MethodId a.getVtableIndex(name, paramCount)

proc getMethodSignature*(a: Assembly, vid: int): lent MethodSignature =
  ## Returns the name of the method with the given vtable index.

  result = a.methodSignatures[vid]

{.pop.}

proc addProc*(a: Assembly, name: string, paramCount: int,
              chunk: Chunk): lent Procedure =
  ## Adds a bytecode procedure to the assembly.

  var p = Procedure(
    id: a.procedures.len,
    name: name, paramCount: paramCount,
    kind: pkBytecode, chunk: chunk
  )
  a.procedures.add(p)
  result = a.procedures[p.id]

proc addProc*(a: Assembly, name: string, paramCount: int,
              impl: RawProcedureImpl): lent Procedure =
  ## Adds a native procedure to the assembly.

  var p = Procedure(
    id: a.procedures.len,
    name: name, paramCount: paramCount,
    kind: pkNative, impl: impl,
  )
  a.procedures.add(p)
  result = a.procedures[p.id]

proc addMethod*(a: Assembly, vtable: int, name: string, paramCount: int,
                impl: RawProcedureImpl) =
  ## Adds a bytecode method to the given vtable in the given assembly.

  a.vtables[vtable].addMethod(a.getVtableIndex(name, paramCount),
                              name, paramCount, impl)

proc addVtable*(a: Assembly, name: string): int =
  ## Adds a new, empty vtable to the given assembly.

  result = a.vtables.len
  a.vtables.add(Vtable(name: name))

# module

proc newModule*(mainChunk: Chunk): Module =
  ## Creates and initializes a new module with the given main chunk.
  Module(mainChunk: mainChunk)

proc addProc*(m: Module, a: Assembly, name: string, paramCount: int,
              impl: RawProcedureImpl) =
  ## Adds a native procedure to the assembly and the module.

  var sym = newSymbol(skProc, identNode(name))
  sym.procId = a.addProc(name, paramCount, impl).id
  m.globalSyms[name] = sym

proc importAll*(m, other: Module) =
  ## Imports all symbols from the other module into ``m``.

  for k, sym in other.globalSyms:
    m.globalSyms[k] = sym


# chunk

proc newChunk*(filename: FilenameId): Chunk =
  ## Creates and initializes a new, empty chunk of bytecode.
  Chunk(filename: filename)

proc setCurrentLineInfo*(chunk: Chunk, lineInfo: LineInfo) =
  ## Sets the line info to use when emitting code.
  chunk.currentLineInfo = lineInfo

proc addLineInfo(chunk: Chunk, n: int) =
  ## Adds n line info entries to the chunk.

  if chunk.codedLineInfos.len > 0 and
     chunk.codedLineInfos[^1].lineInfo == chunk.currentLineInfo:
    inc(chunk.codedLineInfos[^1].runLength, n)
    return

  chunk.codedLineInfos.add((chunk.currentLineInfo, n))

proc getLineInfo*(chunk: Chunk, i: int): LineInfo =
  ## Gets the line info at position ``i`` in the chunk.
  ## **Warning: this is a really slow operation, use sparsely.**

  var n = 0
  for (lineInfo, runLength) in chunk.codedLineInfos:
    for r in 1..runLength:
      if n == i:
        return lineInfo
      inc n

{.push inline.}

proc emitU8*(chunk: Chunk, x: uint8) =
  ## Emits an 8-bit unsigned integer.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add(x)

proc emitU16*(chunk: Chunk, x: uint16) =
  ## Emits a 16-bit unsigned integer.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16)
  ])

proc emitU32*(chunk: Chunk, x: uint32) =
  ## Emits a 32-bit unsigned integer.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16),
    uint8 x.bitsliced(16..<24),
    uint8 x.bitsliced(24..<32),
  ])

proc emitU64*(chunk: Chunk, x: uint64) =
  ## Emits a 64-bit unsigned integer.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16),
    uint8 x.bitsliced(16..<24),
    uint8 x.bitsliced(24..<32),
    uint8 x.bitsliced(32..<40),
    uint8 x.bitsliced(40..<48),
    uint8 x.bitsliced(48..<56),
    uint8 x.bitsliced(56..<64),
  ])

proc emitOpcode*(chunk: Chunk, x: Opcode) =
  ## Emits an opcode.

  chunk.emitU8(x.uint8)

proc emitFloat*(chunk: Chunk, x: float64) =
  ## Emits a 64-bit float.

  chunk.emitU64(cast[uint64](x))

proc emitString*(chunk: Chunk, x: string) =
  ## Emits a length-prepended string.

  # ``x`` must be shorter than ``high(uint32)`` characters, but I can't really
  # imagine anyone making a string literal *that* big.
  # also, this can be made more efficient by creating a chunk-wide table of
  # string literals, but i'm yet to find a good use case for this. in most cases
  # strings are short enough that the benefit is negligible, and very long
  # strings don't really repeat throughout the module, so creating such an
  # optimization doesn't really make much sense.

  chunk.emitU32(x.len.uint32)
  for c in x:
    chunk.emitU8(cast[uint8](c))

proc emitJump*(chunk: Chunk, opcode: JumpOpcode): int =
  ## Emits an empty jump instruction, returns its offset in the chunk.

  result = chunk.bytecode.len
  chunk.emitOpcode(opcode)
  chunk.emitU16(0)

proc patchJump*(chunk: Chunk, i: int) =
  ## Patches the jump at ``i`` with the distance between the jump opcode and the
  ## current position in the chunk.

  # quick math note for anyone wanting to implement backpatching:
  #  - i is the position of the first byte in the jump (the opcode)
  #  - bytecode.len is the position of the byte after the last operand
  #  - the size of a jump instruction is 3 (Opcode + uint16)
  #  - to get the distance between the current position and the opcode,
  #    we subtract bytecode.len - i but also 3 to account for the fact that
  #    we're past the last opcode already
  # ...
  # actually wtf i have no clue how this works but it works so don't touch it
  let distance = chunk.bytecode.len - i - 3
  chunk.bytecode[i + 1] = uint8 distance.bitsliced(0..<8)
  chunk.bytecode[i + 2] = uint8 distance.bitsliced(8..<16)

proc readU8*(chunk: Chunk, i: var int): uint8 =
  ## Reads a uint8 at the given position.

  result = chunk.bytecode[i]
  inc i, uint8.sizeof

proc readU16*(chunk: Chunk, i: var int): uint16 =
  ## Reads a uint16 at the given position.

  result =
    chunk.bytecode[i].uint16 or
    (chunk.bytecode[i + 1].uint16 shl 8)
  inc i, uint16.sizeof

proc readU32*(chunk: Chunk, i: var int): uint32 =
  ## Reads a uint32 at the given position.

  result =
    chunk.bytecode[i].uint32 or
    (chunk.bytecode[i + 1].uint32 shl 8) or
    (chunk.bytecode[i + 2].uint32 shl 16) or
    (chunk.bytecode[i + 3].uint32 shl 24)
  inc i, uint32.sizeof

proc readU64*(chunk: Chunk, i: var int): uint64 =
  ## Reads a uint64 at the given position.

  result =
    chunk.bytecode[i].uint64 or
    (chunk.bytecode[i + 1].uint64 shl 8) or
    (chunk.bytecode[i + 2].uint64 shl 16) or
    (chunk.bytecode[i + 3].uint64 shl 24) or
    (chunk.bytecode[i + 4].uint64 shl 32) or
    (chunk.bytecode[i + 5].uint64 shl 40) or
    (chunk.bytecode[i + 6].uint64 shl 48) or
    (chunk.bytecode[i + 7].uint64 shl 56)
  inc i, uint64.sizeof

proc readOpcode*(chunk: Chunk, i: var int): Opcode =
  ## Reads an opcode at the given position.

  result = chunk.bytecode[i].Opcode
  inc i

proc readFloat*(chunk: Chunk, i: var int): float64 =
  ## Reads a float64 at the given position.

  cast[float64](chunk.readU64(i))

proc readString*(chunk: Chunk, i: var int): string =
  ## Reads the string at the given position.

  let len = chunk.readU32(i).int
  result = newString(len)
  copyMem(result[0].addr, chunk.bytecode[i].addr, len)

  inc i, len

{.pop.}

