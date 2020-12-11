## tsuki's standard library.

import chunk
import common
import value
import vm

proc addSystemModule*(cs: CompilerState, a: Assembly): Module =

  var
    filenameId = cs.addFilename("system.tsu")
    chunk = newChunk(filenameId)
    m = newModule(chunk)

  let mDollar = a.getMethodId("$", 0)

  #---
  # toplevel
  #---

  # echo(_)
  m.addProc a, "echo", 1,
    wrap proc (s: State, args: openArray[Value]): Value =
      let str = s.call(mDollar, args[0])
      s.assert(str.isString, "echo expects a string")
      echo str.getString
      tsukiNil

  # abort(_)
  m.addProc a, "abort", 1,
    wrap proc (s: State, args: openArray[Value]): Value =
      s.assert(args[0].isString, "abort expects an error message string")
      s.setError(args[0].getString)

  #---
  # Nil
  #---

  # not
  a.addMethod vtableNil, "not", 0,
    wrap proc (s: State, args: openArray[Value]): Value =
      tsukiTrue

  # $
  a.addMethod vTableNil, "$", 0,
    wrap proc (s: State, args: openArray[Value]): Value =
      "nil"

  #---
  # Bool
  #---

  # not
  a.addMethod vtableBool, "not", 0,
    wrap proc (s: State, args: openArray[Value]): Value =
      if args[0].isTrue: tsukiFalse
      else: tsukiTrue

  # $
  a.addMethod vtableBool, "$", 0,
    wrap proc (s: State, args: openArray[Value]): Value =
      if args[0].isTrue: "true"
      else: "false"

  #--
  # Float
  #--

  # arithmetic

  a.addMethod vtableFloat, "-", 0,
    wrap proc (s: State, args: openArray[Value]): Value =
      -args[0].getFloat

  block binary:
    template binaryArithm(name: string, op: untyped) =
      a.addMethod vtableFloat, name, 1,
        proc (s: State, args: openArray[Value]): Value =
          s.assert(args[1].isFloat, "second operand must be a Float")
          `op`(args[0].getFloat, args[1].getFloat)

    binaryArithm "+", `+`
    binaryArithm "-", `-`
    binaryArithm "*", `*`
    binaryArithm "/", `/`

  # $
  a.addMethod vtableFloat, "$", 0,
    wrap proc (s: State, args: openArray[Value]): Value =
      $args[0].getFloat

  #--
  # String
  #--

  # $
  a.addMethod vtableString, "$", 0,
    wrap proc (s: State, args: openArray[Value]): Value =
      args[0].getString

  result = m
