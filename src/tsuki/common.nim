import std/tables

type
  FilenameId* = distinct int

  CompilerState* = ref object
    ## Compiler state, used to avoid copying string all around nodes/tokens
    filenames: seq[string]

proc `==`*(a, b: FilenameId): bool {.borrow.}

proc addFilename*(cs: CompilerState, filename: string): FilenameId =
  ## Adds a filename to the state and returns its ID.
  result = FilenameId(cs.filenames.len)
  cs.filenames.add(filename)

proc getFilename*(cs: CompilerState, id: FilenameId): string =
  ## Returns the filename with the given ID.
  cs.filenames[int(id)]
