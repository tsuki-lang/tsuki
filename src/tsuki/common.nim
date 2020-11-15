import std/tables

type
  FilenameId* = distinct int

  CompilerState* = ref object
    # this is used to avoid copying string all around nodes/tokens
    filenames: seq[string]

proc `==`*(a, b: FilenameId): bool {.borrow.}

proc addFilename*(cs: CompilerState, filename: string): FilenameId =
  result = FilenameId(cs.filenames.len)
  cs.filenames.add(filename)

proc getFilename*(cs: CompilerState, id: FilenameId): string =
  cs.filenames[int(id)]
