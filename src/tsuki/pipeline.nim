import chunk
import codegen
import common
import lexer
import parser
import tsukilib
import value
import vm

export chunk
export CompileError
export ParseError
export value
export vm

type
  Pipeline* = object
    ## An object defining how scripts are to be compiled.
    cs*: CompilerState
    a: Assembly

    defaultImports: seq[Module]

proc initPipeline*(): Pipeline =
  ## Creates and initializes a new pipeline.

  new result.cs
  result.a = newAssembly()

{.push inline.}

proc assembly*(p: Pipeline): Assembly =
  ## Returns the assembly from the pipeline.
  p.a

proc addDefaultImport*(p: var Pipeline, m: Module) =
  ## Adds a module that's imported by default in all modules compiled
  ## subsequently.
  p.defaultImports.add(m)

proc addSystem*(p: var Pipeline) =
  ## Adds the ``system`` module to default imports.
  p.addDefaultImport(addSystemModule(p.cs, p.a))

{.pop.}

proc compile*(p: var Pipeline, filename, source: string): Module =
  ## Compiles a module from a source file.

  # parsing
  let filenameId = p.cs.addFilename(filename)
  var lexer = initLexer(p.cs, filenameId, source)
  let ast = lexer.parseScript()

  # codegen
  let chunk = newChunk(filenameId)
  result = newModule(mainChunk = chunk)
  let cg = newCodeGen(p.cs, p.a, result, chunk)

  for m in p.defaultImports:
    result.importAll(m)

  cg.genScript(ast)

proc newState*(p: var Pipeline): State =
  ## Creates a state for executing code from the given pipeline.
  newState(p.cs, p.a)

proc run*(p: var Pipeline, s: State, m: Module): Value =
  ## Runs all top-level code from a module in the given state.
  ## Returns the interpret result of the module.
  s.interpret(m.mainChunk)
