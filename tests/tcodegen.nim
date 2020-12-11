# tests for codegen

import std/sugar

import tsuki/chunk
import tsuki/codegen
import tsuki/common
import tsuki/disassembler
import tsuki/parser
import tsuki/lexer

proc dumpCodegen(test, input: string) =

  echo "----- ", test
  try:
    var
      cs = new(CompilerState)
      filenameId = cs.addFilename("test.tsu")
      lexer = initLexer(cs, filenameId, input)
      ast = lexer.parseScript()

      assembly = new(Assembly)
      chunk = newChunk(filenameId)
      module = newModule(mainChunk = chunk)
      cg = initCodeGen(cs, assembly, module, chunk)

    cg.genScript(ast)
    echo chunk.disassemble()

  except ValueError as e:
    echo e.msg

dumpCodegen "variables and scope", """
  var a = 123
  block
    var b = 12345
    var c = 67890
    var d = a
    var e = b
  end
  var b = a
"""

dumpCodegen "calls", """
  var x = 1 + 2
  var y = not true
  # var z = abc(123)
  var w = abc.def(32)
  var a = not false
  var b = a.invert
"""

dumpCodegen "control flow/if", """
  if true
    var a = 1 + 2
  end
"""

dumpCodegen "control flow/if..elif", """
  if true
    var a = 1 + 2
  elif true
    var b = 3 + 4
  end
"""

dumpCodegen "control flow/if..else", """
  if true
    var a = 1 + 2
  else
    var b = 3 + 4
  end
"""

dumpCodegen "control flow/if..elif..else", """
  if true
    var a = 1 + 2
  elif true
    var b = 3 + 4
  else
    var c = 5 + 6
  end
"""

dumpCodegen "control flow/while", """
  var a = true
  while a
    var b = 10
  end
"""
