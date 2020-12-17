# tsuki 月

tsuki (jap. 月, *moon*) is a lightweight, dynamically typed, modular, embeddable
scripting language for Nim, inspired by Lua, Ruby, and Nim itself.

```
object Greeter = greetings

impl Greeter

  proc greeting(target) =>
    "Hello, " & target & "!"

  proc greet(target)
    echo(.greeting(target))
    .greetings = .greetings + 1
  end

end

proc newGreeter =>
  Greeter { greetings = 0 }

var g = newGreeter()
g.greet("Nimions")
```

- **It's lightweight.** One of tsuki's primary goals is having a language that's
  far more lightweight than NimScript, but at the same time not as ascetic
  as Lua, or rigid as Wren.
- **It's dynamically typed.** Static typing adds a lot of complexities to the
  implementation, such as generics, which quickly get very hairy and add lots of
  dark corners for potential bugs to sneak in. Thus, tsuki avoids it all
  completely. Future plans include an *optional, external* static type checker.
- **It's expressive.** tsuki is built to support multiple different programming
  paradigms. Object-oriented, procedural, functional, pick your flavor.
- **It's embeddable.** As hinted by the previous points, tsuki is designed to
  live in host programs, such as games, desktop applications, window managers,
  and so on.
- **It's modular.** The tsuki pipeline is built to be modular and pluggable.
  You don't need the compiler to run code in the VM. This also helps thread
  safety: multiple VM states may execute code from the same assembly in
  parallel. Additionally, the VM is fully reentrant – tsuki can call Nim procs
  which call tsuki procs which call Nim procs… and so on.

## Roadmap

The following is a roadmap for feature support:

- [x] variables
- [x] control flow - `if`, `while`, `for`
- [x] procedures
  - [ ] closures
- [x] objects
- [ ] modules, `import`
- [ ] standard library
- [ ] `tsuki/pipeline` – simplified compilation and execution pipeline
- [ ] `tsuki/wrap` – automatic Nim code wrapper macro

0.1.0 will be released after all the features are complete and a fairly
comprehensive test suite is in place. The following releases will add support
for extra syntax sugar and language features, to be announced.
