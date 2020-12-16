# tests for codegen

import std/strutils

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
      cg = newCodeGen(cs, assembly, module, chunk)
      system = addSystemModule(cs, assembly)

      state = newState(cs, assembly)

    module.importAll(system)
    cg.genScript(ast)

    echo chunk.disassemble(assembly)
    for i, p in assembly.procedures:
      echo "* procedure ", i, ": ", $(p.name, p.paramCount).MethodSignature
      if p.kind == pkBytecode:
        echo p.chunk.disassemble(assembly).indent(2)

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

run "control flow/if/expression", """
  var cond =
    if false
      "hi"
    else
      "bye"
    end
  echo(cond)
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
  block
    var iter = (1..10)._iterate
    var x = nil
    while iter._hasNext
      x = iter._next
      block
        echo(x)
      end
    end
  end
"""

run "for loops/transformed", """
  for x in 1..10
    echo(x)
  end
"""

run "loops/break", """
  var a = 0
  while true
    var x = a + 10
    var y = x + 20
    a = a + 1
    echo(a)
    echo(x)
    echo(y)
    if a == 10
      var z = 10
      break
    end
  end
"""

run "loops/continue", """
  for a in 1..10
    var x = 1
    if a == 5
      var z = 20
      continue
    end
    echo(a)
  end
"""

run "procs/basic definition", """
  proc sayHello
    echo("hello")
  end

  sayHello()
"""

run "procs/with parameters", """
  proc doThings(a, b, c)
    echo(a + b * c)
  end

  doThings(1, 2, 3)
"""

run "procs/implicit result", """
  proc nop end
  echo(nop())

  proc fac(n)
    result = 1
    for i in 1..n
      echo(i)
      result = result * i
    end
  end

  echo(fac(10))
"""

run "procs/recursive", """
  proc fib(n)
    result =
      if   n == 0  0
      elif n == 1  1
      else         fib(n - 1) + fib(n - 2)
      end
  end

  for i in 1..10
    echo(fib(i))
  end
"""

echo getTotalMem()
