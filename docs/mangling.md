# Some notes on mangling

This document outlines some considerations for implementing mangling inside the compiler.

## LLVM backend

I'd prefer if mangled names were descriptive and human-readable, rather than overly mangled to the point where no human being is able to understand them (looking at you, C++).

Specifics:
- LLVM functions can be called whatever we want, there's no limit on which characters we can or cannot use.
- Rust strings must be valid UTF-8.

This leads to the following scheme:
- `module`, `module.<path>`, or `package.module.<path>`
  - where `<path>` is one of the following:
    - `<fun>` - eg. `function_name`, `blah1` - a valid function name (in `snake_case`)
    - `<type>.<fun>` - eg. `MyObject.function` - an instance function in a type
    - `type:<type>.<fun>` - eg. `type:MyObject.new` - an associated function
    - `<type>.[<trait>].<fun>` - eg. `MyObject.[As[Int]].convert` - a function associated with a trait
    - `{<integer>}` - eg. `{0}` - anonymous functions, eg. closures and do-blocks
  - Paths may nest freely. `module.function.local_function` is a perfectly valid path, specifying a locally-scoped function `local_function` inside the function `function` inside the module `module`.

# Stack traces

Function names mangled using this scheme should _never_ appear in stack traces, as it leads to a terrible user experience. They get overly long and hard to read, hence I propose to drop the module's name. After all, it's already obvious from the filename.

For instance, if we have a stack trace for package `test`, module `hello`, this stack trace:
```
Stack traceback:
  std:panics.tsu 234:4         std.panics.panic
  test:hello.tsu 4:2           test.hello.my_fallible_function
  test:hello.tsu 6:1           test.hello
```
should get turned into:
```
Stack traceback:
  std:panics.tsu 234:4         panic
  test:hello.tsu 4:2           my_fallible_function
  test:hello.tsu 6:1           {module code}
```

Stack traces should be compact and readable at a glance. We don't need to list _every_ single path piece. This is something most compilers nowadays get wrong.

Everything after the module name should remain as-is, and the empty string previously containing only the module name should be replaced with `{module code}`.

**UX above all else**.

## Some deets on the stack trace's formatting

The paths shown in the stack trace should never be full paths to the `src` directory. Instead, they should be `<package name>:<path to file inside of src>`. Not sure about this, but maybe the `.tsu` extension should also be dropped?

The full format is the following:
```
Stack traceback:
  <file> <line>:<column>        <short function name>
  ...
```
