import std/strutils

import chunk
import common
import errors
import lexer
import value

type
  State* = ref object of BaseState
    ## Execution state. This is what contains the actual values for globals,
    ## and also some interpreting-related things, such as the call stack.
    ##
    ## **Warning:** This is *NOT* thread-safe! If you plan on using tsuki in a
    ## multi-threaded application, every thread must get its own tsuki State.
    ## However, in most cases this isn't really a problem because the only thing
    ## a State contains are globals and the call stack, which you don't want to
    ## share between threads anyways.
    cs*: CompilerState
    a*: Assembly
    globals: seq[Value]
    callStack: seq[StackFrame]
    nativeError: string

  StackFrame = object
    chunk: Chunk
    stackBottom: int
    pc, errpc: int  # errpc is used for error reporting
    procName: string

  StackEntry* = tuple
    native: bool
    filename: FilenameId
    lineInfo: LineInfo
    procName: string

  InterpretError* = object of CatchableError
    cs*: CompilerState
    callStack*: seq[StackEntry]
    error*: string

  NativeProcedure* = proc (s: State, args: openArray[Value]): Value


# state

proc newState*(cs: CompilerState, a: Assembly): State =
  ## Creates and initializes a new execution state.
  State(cs: cs, a: a)

{.push inline.}

proc getGlobal*(s: State, i: int): var Value =
  ## Returns the global with the given index.
  s.globals[i]

proc setGlobal*(s: State, i: int, v: sink Value) =
  ## Sets the global with the given index.

  if i notin 0..<s.globals.len:
    s.globals.setLen(i + 1)
  s.globals[i] = v

proc setError*(s: State, msg: string) =
  ## Sets the error message in the state. This then aborts execution in the VM.
  s.nativeError = msg

proc getError*(s: State): string =
  ## Returns the error message in the state.
  s.nativeError

{.pop.}


# utilities

template assert*(s: State, cond: bool, msg: string) =
  ## Sets the error to ``msg`` and returns if ``cond`` is ``false``.

  if not cond:
    s.setError(msg)
    return

proc addFormattedError(dest: var string, ie: InterpretError) =
  ## Formats the error nicely.

  dest.add("Error: ")
  dest.add(ie.error)

  for se in ie.callStack:
    dest.add("\n  ")

    var lineInfo = ""
    if se.native:
      lineInfo.add("[Nim proc]")
    else:
      lineInfo.addf(exceptionFormat, [
        ie.cs.getFilename(se.filename),
        $se.lineInfo.line, $se.lineInfo.column,
      ])
    while lineInfo.len < 24:
      lineInfo.add(' ')
    lineInfo.add("  ")
    lineInfo.add(se.procName)

    dest.add(lineInfo)

proc wrap*(p: NativeProcedure): RawProcedureImpl =
  ## Wrapping of proc implementations to raw proc implementations.
  ##
  ## **EXTREME Warning!** Due to a regression introduced with Nim 1.4
  ## (see nim-lang/Nim#16325) you must be *very careful* with registering
  ## NativeProcedures, as now the implicit conversion between a RawProcedureImpl
  ## and NativeProcedure is legal which causes codegen bugs. You should be using
  ## the high-level API anyways, though.
  (proc (s: BaseState, args: openArray[Value]): Value =
    result = p(s.State, args))


# the VM

proc interpret*(s: State, procedure: Procedure, args: seq[Value],
                isMethodCall: bool): Value =
  ## Interprets a Procedure. This is the heart of the VM.

  var
    c: Chunk
    pc, errpc = 0
    stack: seq[Value] = args
    stackBottom = 0
    procName = procedure.name

  template local(i: int): var Value =
    stack[stackBottom + i]

  template storeFrame() =
    s.callStack.add(StackFrame(
      chunk: c,
      stackBottom: stackBottom,
      pc: pc, errpc: errpc,
      procName: procName,
    ))

  template restoreFrame() =
    let f = s.callStack.pop()
    c = f.chunk
    pc = f.pc
    stackBottom = f.stackBottom
    procName = f.procName

  template abort(message: string) =
    var e = new(InterpretError)
    e.error = message
    e.cs = s.cs

    for f in s.callStack:
      var entry: StackEntry
      entry.procName = f.procName
      if f.chunk.isNil:
        entry.native = true
      else:
        entry.filename = f.chunk.filename
        entry.lineInfo = f.chunk.getLineInfo(errpc)
      e.callStack.add(entry)

    e.msg.addFormattedError(e[])
    raise e

  template doCall(p: Procedure, isMethodCall: bool) =
    c = nil
    procName = p.name

    case p.kind
    of pkNative:
      let result =
        p.impl(s, stack.toOpenArray(stackBottom, stack.high))

      # check error state
      if s.getError().len > 0:
        storeFrame()
        abort(s.getError())

      # remove params from the stack
      stack.setLen(stack.len - p.paramCount - ord(isMethodCall))
      stack.add(result)
    of pkBytecode:
      unreachable "bytecode proc calls are NYI"

  case procedure.kind
  of pkNative:
    doCall(procedure, isMethodCall)
  of pkBytecode:
    c = procedure.chunk
    while true:
      {.computedGoto.}

      errpc = pc
      let opcode = c.readOpcode(pc)

      case opcode

      of opcPushNil:
        stack.add(tsukiNil)

      of opcPushTrue:
        stack.add(tsukiTrue)

      of opcPushFalse:
        stack.add(tsukiFalse)

      of opcPushFloat:
        stack.add(c.readFloat(pc))

      of opcPushString:
        stack.add(c.readString(pc))

      of opcPushGlobal:
        stack.add(s.getGlobal(int c.readU16(pc)))

      of opcPopToGlobal:
        s.setGlobal(int c.readU16(pc), stack.pop())

      of opcAssignToGlobal:
        let
          gid = int c.readU16(pc)
          old = s.globals[gid]
        s.globals[gid] = stack[^1]
        stack[^1] = old

      of opcPushLocal:
        stack.add(local(int c.readU16(pc)))

      of opcPopToLocal:
        local(int c.readU16(pc)) = stack.pop()

      of opcAssignToLocal:
        let
          lid = int c.readU16(pc)
          old = local(lid)
        local(lid) = stack[^1]
        stack[^1] = old

      of opcDiscard:
        assert stack.len > 0, $pc
        stack.setLen(stack.len - c.readU8(pc).int)

      of opcJumpFwd:
        pc += c.readU16(pc).int

      of opcJumpFwdIfFalsey:
        pc += c.readU16(pc).int * ord(stack[^1].isFalsey)

      of opcJumpBack:
        pc -= c.readU16(pc).int

      of opcCallProc:

        let p = s.a.procedures[c.readU16(pc)]
        storeFrame()

        # adjust stack bottom to match the proc's signature
        stackBottom = stack.len - p.paramCount

        # do the call
        doCall(p, isMethodCall = false)
        restoreFrame()

      of opcCallMethod:

        let
          vid = int c.readU16(pc)
          sig = s.a.getMethodSignature(vid)
        storeFrame()
        stackBottom = stack.len - sig.paramCount - 1

        let
          receiver = local(0)
          vtable = s.a.vtables[receiver.vtable]

        if vtable.hasMethod(vid):
          # do the call
          let m = vtable.getMethod(vid)
          doCall(m, isMethodCall = true)
        else:
          abort('\'' & vtable.name & "' does not respond to '" & $sig & '\'')
        restoreFrame()

      of opcHalt: break

  assert stack.len in [0, 1], "stack must be empty or have a single result"
  result =
    case stack.len
    of 0: tsukiNil
    of 1: stack[0]
    else: unreachable

proc interpret*(s: State, chunk: Chunk, name: string = "<main chunk>"): Value =
  ## Interprets the given chunk with the specified name to use in stack traces.

  result = s.interpret(Procedure(
    name: name,
    kind: pkBytecode,
    chunk: chunk,
  ), args = @[], isMethodCall = false)

proc call*(s: State, mid: MethodId, args: varargs[Value]): Value =
  ## Calls the method with the given ID passing the given arguments to it.
  ## The first argument must always be present and is the receiver of
  ## the method.
  ## Note that this only works for bytecode methods.

  assert args.len > 0, "call receiver must always be present"

  let
    vtable = s.a.vtables[args[0].vtable]
    procedure = vtable.getMethod(mid.int)

  s.interpret(procedure, @args, isMethodCall = true)
