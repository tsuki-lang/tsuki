# tests for codegen

import std/sugar

import tsuki/chunk
import tsuki/codegen
import tsuki/common
import tsuki/disassembler
import tsuki/parser
import tsuki/lexer
import tsuki/tsukilib
import tsuki/value
import tsuki/vm

proc run(test, input: string) =

  echo "----- ", test
  try:
    var
      cs = new(CompilerState)
      filenameId = cs.addFilename("test.tsu")
      lexer = initLexer(cs, filenameId, input)
      ast = lexer.parseScript()

      assembly = newAssembly()
      chunk = newChunk(filenameId)
      module = newModule(mainChunk = chunk)
      cg = initCodeGen(cs, assembly, module, chunk)
      system = addSystemModule(cs, assembly)

      state = newState(cs, assembly)

    module.importAll(system)
    cg.genScript(ast)
    echo chunk.disassemble(assembly)
    echo "<interpret result> ", state.interpret(chunk)

  except ValueError as e:
    echo e.msg
    echo "COMPILE FAILED"
  except InterpretError as e:
    echo e.msg
    echo "INTERPRET FAILED"

  echo()

run "variables and scope", """
  var a = 123
  block
    var b = 12345
    var c = 67890
    var d = a
    var e = b
  end
  var b = a
"""

run "nested scopes", """
  block
    var a = 1
    block
      var b = 2
      block
        var c = 3
        block
          echo(a)
          echo(b)
          echo(c)
        end
      end
    end
  end
"""

run "calls/proc/native", """
  echo("this is a test")
"""

run "calls/method/native", """
  var a = not true
  var b = not nil
  echo(a)
  echo(b)
"""

run "calls/method/reentrant", """
  echo(2)
"""

run "lib/arithmetic", """
  var a = -1
  var b = 1 + 2
  var c = 4 - 2
  var d = 1 * 2
  var e = 1 / 2
  echo(a)
  echo(b)
  echo(c)
  echo(d)
  echo(e)
"""

run "control flow/if", """
  if true
    echo(1 + 2)
  end
"""

run "control flow/if..elif", """
  if true
    echo(1 + 2)
  elif true
    echo(3 + 4)
  end
"""

run "control flow/if..else", """
  if true
    echo(1 + 2)
  else
    echo(3 + 4)
  end
"""

run "control flow/if..elif..else", """
  if true
    echo(1 + 2)
  elif true
    echo(3 + 4)
  else
    echo(5 + 6)
  end
"""

run "control flow/while", """
  var a = 0
  while a < 10
    echo(a)
    a = a + 1
  end
"""

run "nim data", """
  var r = 1..10
  echo(r)
  echo(0..<10)
"""

run "for loops/experimental sample", """
  var range = 1..10
  var iter = range._iterate
  while iter._hasNext
    var x = iter._next
    block
      echo(x)
    end
  end
"""

run "for loops/transformed", """
  for x in 1..10
    echo(x)
  end
"""
