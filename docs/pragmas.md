# Pragmas specific to this implementation

Different implementations may define their own sets of pragmas for implementing the standard library. This reference implementation of the compiler and standard library use the following pragmas.

#### `compiler_builtin_type(type_identifier: Atom)`

The `compiler_builtin_type` pragma may be used on `type` definitions without an `=` sign after the name, to bind builtin types to names. The `type_identifier` atom may be one of the following values, corresponding to the following built-in types:

| Atom value | Type |
| --- | --- |
| `:noreturn` | `NoReturn` |
| `:bool` | `Bool` |
| `:int8` | `Int8` |
| `:int16` | `Int16` |
| `:int32` | `Int32` |
| `:int64` | `Int64` |
| `:uint8` | `Uint8` |
| `:uint16` | `Uint16` |
| `:uint32` | `Uint32` |
| `:uint64` | `Uint64` |
| `:float32` | `Float32` |
| `:float64` | `Float64` |
| `:size` | `Size` |

Examples:

```
type NoReturn :: compiler_builtin_type(:noreturn)
type Size :: compiler_builtin_type(:size)
```

These type definitions can be found in `std/std.tsu`, which is imported implicitly into each module.
