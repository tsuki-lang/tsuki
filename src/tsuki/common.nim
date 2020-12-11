type
  FilenameId* = distinct int

  CompilerState* = ref object
    ## Compiler state, used to avoid copying string all around nodes/tokens
    filenames: seq[string]

  UnreachableDefect* = object of AssertionDefect

proc `==`*(a, b: FilenameId): bool {.borrow.}

proc addFilename*(cs: CompilerState, filename: string): FilenameId =
  ## Adds a filename to the state and returns its ID.
  result = FilenameId(cs.filenames.len)
  cs.filenames.add(filename)

proc getFilename*(cs: CompilerState, id: FilenameId): string =
  ## Returns the filename with the given ID.
  cs.filenames[int(id)]

template unreachable*(msg: string = "unreachable code reached") =
  ## Raises an UnreachableDefect if the code path is reached.

  # i really wish this was in the stdlib
  raise newException(UnreachableDefect, msg)
