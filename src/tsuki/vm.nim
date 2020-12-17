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

proc setError*(s: State, msg: sink string) =
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

when defined(tsukiDebugStack):
  import std/strutils

proc interpret*(s: State, procedure: Procedure, args: seq[Value],
                isMethodCall: bool): Value =
  ## Interprets a Procedure. This is the heart of the VM.

  var
    c: Chunk
    pc, errpc = 0
    stack: seq[Value] = args
    stackBottom = 0
    procName = procedure.name

  when defined(tsukiDebugStack):
    template xpc: string = errpc.BiggestInt.toHex(3)

    proc add(s: var seq[Value], v: Value) =
      system.add(s, v)
      echo xpc, " push -> ", s

    proc pop(s: var seq[Value]): Value =
      result = system.pop(s)
      echo xpc, " pop -> ", s

    proc `[]`(s: var seq[Value], i: int): var Value =
      echo xpc, " get <- ", i, "  ", s
      system.`[]`(s, i)

    proc `[]=`(s: var seq[Value], i: int, v: Value) =
      echo xpc, " set <- ", i, "  ", s, " <- ", v
      system.`[]=`(s, i, v)

    proc `[]`(s: var seq[Value], i: BackwardsIndex): var Value =
      echo xpc, " get^ <- ", i.int, "  ", s
      system.`[]`(s, i)

    proc `[]=`(s: var seq[Value], i: BackwardsIndex, v: Value) =
      echo xpc, " set^ <- ", i.int, "  ", s, " <- ", v
      system.`[]=`(s, i, v)

    proc setLen(s: var seq[Value], newlen: int) =
      let oldlen = s.len
      echo xpc, " setLen -> ", newlen, "  change: ", newlen - oldlen
      system.setLen(s, newlen)

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
    # discard locals/params/everything from the current stack frame
    stack.setLen(stackBottom)

    # restore the running chunk, pc, stack bottom, proc name
    let f = s.callStack.pop()
    c = f.chunk
    pc = f.pc
    stackBottom = f.stackBottom
    procName = f.procName

  template abort(message: string) =
    storeFrame()

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
        entry.lineInfo = f.chunk.getLineInfo(f.errpc)
      e.callStack.add(entry)

    e.msg.addFormattedError(e[])
    raise e

  template doCall(p: Procedure) =
    storeFrame()
    c = nil
    procName = p.name
    stackBottom = stack.len - p.paramCount

    case p.kind
    of pkNative:
      let result =
        p.impl(s, stack.toOpenArray(stackBottom, stack.high))

      # check error state
      if s.getError().len > 0:
        abort(s.getError())

      # restore the previous stack frame
      restoreFrame()

      # store the new result at the top
      stack.add(result)

    of pkBytecode:
      # as simple as it could get.
      # the stack frame gets restored by the opcReturn at the end of each proc
      c = p.chunk
      pc = 0

  case procedure.kind
  of pkNative:
    doCall(procedure)
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

      of opcNewObject:
        let
          vid = int c.readU16(pc)
          size = int c.readU8(pc)
          obj = newObject(vid, size)
        for i in countdown(size - 1, 0):
          obj.getObject[i] = stack.pop()
        stack.add(obj)

      of opcPushField:
        let obj = local(0).getObject
        stack.add(obj[int c.readU8(pc)])

      of opcAssignToField:
        let
          value = stack.pop()
          obj = local(0).getObject
          fid = int c.readU8(pc)
        stack.add(obj[fid])
        obj[fid] = value

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
        doCall(p)

      of opcCallMethod:

        let
          vid = int c.readU16(pc)
          sig = s.a.getMethodSignature(vid)

        let
          receiver = stack[stack.len - sig.paramCount]
          vtable = s.a.vtables[receiver.vtable]

        if vtable.hasMethod(vid):
          # do the call
          let m = vtable.getMethod(vid)
          doCall(m)
        else:
          abort('\'' & vtable.name & "' does not respond to '" & $sig & '\'')

      of opcReturn:

        let val = stack[^1]
        restoreFrame()
        stack.add(val)

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

