## Test suite runner.

import std/options
import std/parsecfg
import std/streams
import std/strutils
import std/tables

import tsuki/ast
import tsuki/common
import tsuki/disassembler
import tsuki/lexer
import tsuki/parser
import tsuki/pipeline

type
  Test = object
    name: string
    tests: string
    expectOutput: Option[string]
    source: string

  Suite = object
    tests: OrderedTable[string, Test]

  TestFilter = enum
    tfNone = "none"
    tfFailed = "failed"
    tfOk = "ok"
    tfAll = "all"

proc loadConfig(test: var Test, cfgbuffer: string) =

  var
    stream = newStringStream(cfgbuffer)
    config = loadConfig(stream, filename = "[test config]")
    hadOne = false

  for sectionName, section in config:
    assert not hadOne, "too many sections in a single #. block"

    test.name = sectionName
    test.tests = section["tests"]
    if "expect_output" in section:
      test.expectOutput = some section["expect_output"].strip.dedent

    hadOne = true

proc readSuite(filename: string): Suite =
  ## Reads a test suite from a file.

  var
    test: Test
    cfgbuffer: string

    beforeMode = false
    before: string

  template finishTest() =
    if test.name.len > 0 and test.source.len > 0:
      if not beforeMode:
        test.source = before & test.source
      beforeMode = false
      result.tests[test.name] = test
      test = Test()

  for line in readFile(filename).splitLines:
    if line.startsWith("#. ") or line == "#.":
      finishTest()
      cfgbuffer.add(strip line[2..^1])
      cfgbuffer.add('\n')
    elif line.startsWith("#."):
      case strip line[2..^1]
      of "before":
        beforeMode = true
      of "before off":
        finishTest()
        before.setLen(0)
      else: assert false, "invalid #. directive"
    else:
      if cfgbuffer.len > 0:
        loadConfig(test, cfgbuffer)
        cfgbuffer.setLen(0)
      else:
        if beforeMode:
          before.add(line)
          before.add('\n')
        else:
          test.source.add(line)
          test.source.add('\n')
  finishTest()

template testMatchesFilter(failed: bool, filter: TestFilter, body: untyped) =
  ## Matches failed state against a filter.

  case filter
  of tfNone: discard
  of tfAll:
    `body`
  of tfOk:
    if not failed:
      `body`
  of tfFailed:
    if failed:
      `body`

proc runTest(suite: Suite, name: string,
             dumpAst, disassemble = tfNone): Option[string] =
  ## Runs a test from the suite. Returns an error string, or none in case of
  ## success.

  let test = suite.tests[name]

  var p = initPipeline()
  p.addSystem()

  let mDollar = p.assembly.getMethodId("$", 0)

  var
    testTools = newChunk(p.cs.addFilename("testtools.tsu")).newModule()
    testOutput = ""

  testTools.addProc p.assembly, "output", 1,
    wrap proc (s: State, args: openArray[Value]): Value =
      let maybeStrVal = s.safeCall(mDollar, args[0])
      var str: string
      if maybeStrVal.isSome:
        let strVal = maybeStrVal.get
        s.assert(strVal.isString, "testtools.output expects a string")
        str = strVal.getString
      else:
        str = $args[0]
      if testOutput.len > 0:
        testOutput.add('\n')
      testOutput.add(str)

  p.addDefaultImport(testTools)

  var
    hadParseError = false
    m: Module

  try:
    m = p.compile(filename = "test", test.source)
    let s = p.newState()
    discard p.run(s, m)

    if test.expectOutput.isSome and testOutput != test.expectOutput.get:
      result = some(
        "test output did not match expected output:\n" &
        "  expected: " & test.expectOutput.get.escape & '\n' &
        "  got: " & testOutput.escape
      )

  except ParseError as e:
    result = some e.msg
    hadParseError = true

  except CompileError as e:
    result = some e.msg

  except InterpretError as e:
    result = some e.msg

  except Defect as d:
    result = some("DEFECT: " & d.msg & "\n" & d.getStackTrace)
    hadParseError = true

  testMatchesFilter(result.isSome, dumpAst):
    if not hadParseError:
      var lexer = p.cs.initLexer(p.cs.addFilename("dump"), test.source)
      let ast = lexer.parseScript()
      result.get.add("\n---\nAST dump:\n")
      result.get.add(ast.treeRepr)

  testMatchesFilter(result.isSome, disassemble):
    if not m.isNil:

      result.get.add("\n---\nModule disassembly:\n")
      result.get.add(m.mainChunk.disassemble(p.assembly))

      for i, pr in p.assembly.procedures:
        if pr.kind == pkBytecode:
          result.get.add("\n---\nProcedure " & $i & ": " & pr.name &
                         " disassembly:\n")
          result.get.add(pr.chunk.disassemble(p.assembly))

      for i, vt in p.assembly.vtables:
        result.get.add("\n** Vtable " & $i & ": " & vt.name)
        for i, mtd in vt.methods:
          if mtd.isSome and mtd.get.kind == pkBytecode:
            result.get.add("\n---\nMethod " & $i & mtd.get.name &
                           " disassembly:\n")
            result.get.add(mtd.get.chunk.disassemble(p.assembly))

    else:
      result.get.add("\nModule disassembly not available, possibly because " &
                     "an assertion was triggered in the compiler.")

when isMainModule:

  import std/os
  import std/parseopt
  import std/terminal

  var
    runTests: seq[string]
    dumpAst, disassemble = tfNone
  for kind, key, value in getopt(commandLineParams()):
    case kind
    of cmdArgument:
      runTests.add(key)
    of cmdShortOption, cmdLongOption:
      case key
      of "dumpAst": dumpAst = parseEnum[TestFilter](value)
      of "disassemble": disassemble = parseEnum[TestFilter](value)
      else: quit "invalid option: " & key
    else: assert false

  proc runSuite(filename: string) =

    var
      total, failed = 0
      failedNames: seq[string]
    let suite = readSuite(filename)

    for name, test in suite.tests:
      stderr.styledWriteLine(
        styleBright, fgCyan, "\nTest: ",
        fgWhite, name, "\n", resetStyle,
        styleDim, "  ", test.tests
      )
      let error = suite.runTest(name, dumpAst, disassemble)
      if error.isSome:
        stderr.styledWriteLine(
          fgRed, "  FAILED: \n",
          fgWhite, error.get.indent(4)
        )
        inc failed
        failedNames.add(name)
      else:
        stderr.styledWriteLine(fgGreen, "  OK")
      inc total

    stderr.styledWriteLine(
      "\n",
      fgGreen, "PASSED: ", fgWhite, $(total - failed), "   ",
      fgRed, "FAILED: ", fgWhite, $failed,
    )
    if failedNames.len > 0:
      stderr.styledWriteLine(fgRed, "Tests that failed:")
      for name in failedNames:
        stderr.styledWriteLine(
          "  ", fgYellow, name,
          ": ", fgWhite, suite.tests[name].tests
        )

  for pc, filename in walkDir("tests"):
    if pc in {pcFile, pcLinkToFile} and filename.splitFile.ext == ".tsu":
      stderr.styledWriteLine(
        styleBright, fgMagenta, "Test suite: ",
        fgWhite, filename
      )
      runSuite(filename)
