# tsuki

An elegant, robust, and efficient programming language, that just lets you get things done.

---

# Preamble

Dear reader,

What you're reading here, is my ([@liquidev](https://github.com/liquidev)'s) vision, of what a _fun_ programming language looks like. A programming language that focuses on usability, readability, and sensible defaults, while letting the programmer decide when they need to use advanced, more low-level features, to squeeze out extra performance.

Right now, this document is not much more than a vision for what the language should be. The first compiler is _nowhere_ near complete, but I appreciate that you're reading this informal specification about what I'd like this language to become.

Enjoy programming, and have fun reading.

---

# Lexis

## Encoding

tsuki source code is encoded in UTF-8, using LF (ASCII 0Ah) for line breaks. The Windows convention of CR (ASCII 0Dh) followed by LF is not supported. ASCII 20h is used as the whitespace character for indentation and separating tokens from one another.

The standard extension for tsuki files is `.tsu`, case-sensitive. Using other extensions is an error.

## Comments

Comments in tsuki start with `#` and end with the end of the line:

```
# hello, world!
```

Documentation comments are similar, but begin with two hashes:

```
## this is documentation. hello!
```

Unlike regular comments, doc comments are part of the abstract syntax tree of the language; this means that the parser recognizes them as actual syntax, so doc comments cannot be sprinkled willy-nilly over a program. More about where they are allowed can be found in the "parsing" section of this document.

Doc comments use GitHub Flavored Markdown for documentation, along with some extra doxygen-like directives described later.

## Literals

tsuki has a few primitive type literals that are the foundation of
every program:

```
nil         # no value

true false  # booleans

123         # integers; the appropriate size is inferred depending on context
123_u8      # the size can be enforced by appending _<size>, where size is one of:
            # u8, u16, u32, u64
            # i8, i16, i32, i64
            # the underscore before the size is mandated for readability purposes.
1_000_000   # underscores may be used between digits for clarity
0b000111000 # binary literals
0o1234567   # octal literals
0x123abc    # hex literals

3.1415      # floats; as with integers, size can be provided
3_f32       # if the size isn't provided, it defaults to _f64, and is narrowed
            # down to 32-bits if context requires that
3.1e5       # floats can have exponents attached

:enoent     # atoms; they are valid identifiers prefixed by :

'a' '\n'    # character literals
"hello"     # short string literals
"""hello
world"""    # multiline string literals
            # the following escape sequences may be used:
            # \n  line feed
            # \r  carriage return
            # \t  tabulator
            # \'  the character '
            # \"  the character "
            # other single-line escapes aren't as common, and thus are not supported by
            # the syntax, but they may be substituted with the following generic escape
            # sequence:
            # \xXX      the byte with the hex code XX
            # where X is a hex digit from the set [0-9a-f]
```

## Identifiers

_Identifiers_ start with characters from the set `[a-zA-Z_]`, and may continue with zero or more characters from the set `[a-zA-Z0-9_]`.

A _symbol_ is a _resolved identifier_, that is, an identifier that refers to something within scope, such as an object, or a function, or a variable.

Identifiers in are separated into two groups:

- Type names, written in `PascalCase`,
- Other names, written in `snake_case`.

This is enforced by the compiler to keep a consistent coding style across various codebases.

The following identifiers are reserved as _keywords_:
```
_ and as atom auto box dependency derive do elif else for fun getters if impl
import in is let macro match move not object or pub return self try type uninit
union where while var
```
TODO: This list of keywords is constantly changing as the language spec is refined, and may currently be imprecise. It should be updated once the compiler is finished.

The identifier `_` is special; it's an identifier that is used for ignoring things. Declaring a variable with the name `_` discards its value, and the identifier cannot be used as a valid item name. Additionally, when used as a statement, it's a no-op, and can be used to create empty blocks.

## Blocks

tsuki makes use of significant indentation for program structure. Unlike most programming languages, the indentation level is stored *per token*, which not only simplifies the implementation, but also avoids ugly backslash escapes for continuing lines, and yields more meaningful error messages.

Each leading whitespace character ` ` adds 1 to the indentation level of tokens on a given line. Tabs are not permitted, as they do not render with a consistent width across different text editors, thus causing confusion.

The most simple block is the _do-block_:
```
do
   _ # statements go here
```

The special identifier `_` may be used when a dummy no-op statement is needed, ala `pass` from Python.

The standard indent size is 3 spaces, because 2 spaces is too little to be readable, and 4 spaces is too wide to fit comfortably in 100 columns, which is the standard line width. Different amounts may be used depending on the developer's preferences.

## Operators

tsuki defines a few standard operators.

The following prefix operators are available:
```
not ~ - () [] . ^ ..
```

The following infix operators are available. The list is sorted by precedence, where top is biggest precedence, and bottom is lowest precedence. Lines with more than one operator contain operators of equal precedence.
```
() [] . ^ ?
**
* / << >> & | ^^
+ - ~
.. ..<
== != < > <= >= is in
and
or
= += -= *= /=
```

Most of the listed operators are overloadable, see [Operator overloading](#operator-overloading).

The token `->` appears inside the syntax, but is _not_ an operator - it's only used for punctuation. This token is the "then" arrow, used in control flow expressions to separate the condition from a single-line expression body.

# Parsing

tsuki programs are read in lines from top to bottom, left to right, until the end of a file is reached.

Top-level declarations, as well as declarations within `impl` blocks, are order-independent. Symbols from below can access symbols from above, and vice versa. This does not apply to bodies of functions, where variables declared later in scope cannot be accessed before their declaration.

Similar to scripting languages like Python, regular code may appear outside of a function. This is known as _top-level_ or _module level_ code. Top-level code in a single module is evaluated in the usual order (top to bottom, left to right). Code from other modules is evaluated in the order they're imported from that module, and top-level code in a single module is only evaluated once.

# Expressions

Expressions in tsuki are made up of two types of operations: _prefix_, and _infix_. _Prefixes_ begin an expression, and _infixes_ continue an already existing expression. Consider this very simple example:

```
1 + 2
```
An expression starts with a prefix, and literals are prefixes, so `1` is read. Then, the next token is examined, to determine whether it's a valid infix - in our case `+` is a valid infix, so parsing continues in the `+` rule, which expects another expression on its right-hand side.

This is also where precedence comes in: if it's part of a statement, an expression is parsed with the precedence 0, which is lower than any other precedence level. The precedence level determines which infix operators are treated as part of the current expression; if an operator's precedence level is lower than that of the current expression, it cannot be part of this expression, so it's skipped, and later picked up by an expression with a lower precedence level. This algorithm is known as *Pratt parsing*, or *precedence climbing*.

Infix tokens that continue a line must have an indentation level greater than the first token of the expression:
```
let test = "hello" ~ " world "
   ~ "this" ~ " is " ~ "a "
   ~ "test"
# or
let test = "hello" ~ " world " ~
   "this" ~ " is " ~ "a " ~
   "test"
```

## Prefixes

Prefixes include:
- literals (nil, booleans, integers, floats, atoms, strings)
- identifiers
- prefix operators
- grouping/tuple construction `()`
- array literals `[]`
- `do`, `if`, and `match` expressions
- `try` expressions

## Infixes

Infixes include:
- infix operators
- function calls `()`

## `or` and `and` operators

The `or` and `and` operators are special, non-overloadable operators. They are not overloadable because they perform _short-circuit evaluation_. This means that when the first operand already determines the result of the operation, the second operand will not be evaluated.

In case of `or`, if the first operand is `true`, the second operand is not evaluated, and `true` is returned.

In case of `and`, if the first operand is `false`, the second operand is not evaluated, and `false` is returned.

The `or` operator can be used to safely unwrap an optional or a result while providing a default value, in case a value is not present, or an error occured. For results, the error is discarded.
```
let nope: ?Int = nil
let yes = nope or 1
print(yes)  # 1
```
Following the rule of short-circuit evaluation, the second operand is not evaluated if the value is unwrapped successfully.

`and` can also be used on optionals and results. For instance:
```
# If `update()` fails, its error variant is returned. Otherwise, `draw()` is executed, and if _it_
# fails, _its_ error variant is returned. If both sides succeed, the right side's successful variant
# is returned.
# Note that for this to work, `update` and `draw` must share the same return type.
let test: !() = update() and draw()
# This also works for optionals.
print(Nil[()] and Nil[()])  # Nil
print(Some(()) and Nil[()])  # Nil
print(Some(()) and Some(()))  # Some(())
```

## `do` expressions

The `do` expression evaluates a block of code and returns the result of the last statement in the block.

```
print(do
   let x = 1
   x + 2)  # 3
```

Note that the last statement *must* be an expression statement, as the `do` expression must have a result.

## `if` expressions

The `if..elif..else` expression evaluates a boolean condition, and if it's `true`, evaluates the block of code following that condition. If the condition is `false`, the next condition is evaluated, and so on. If none of the conditions evaluate to `true`, the `else` arm is executed.

```
import @std.io

let name = stdin.read_line?
print(
   if name == "Mark"
      "Oh hi Mark"
   elif name == "Gabe"
      "How's that 3rd game going?"
   else
      "Hello, " ~ name ~ "!")
```

Just like with `do`, the last statement in each arm must be an expression statement.

An `if` expression can be written in a shorthand way by using `->` after the condition.
```
print(
   if name == "Mark" -> "Oh hi Mark"
   elif name == "Gabe" -> "How's that 3rd game going?"
   else -> "Hello, " ~ name ~ "!"
)
```

In some cases the parser may get confused if the starting token of an `if` expression is an infix token:
```
let s =
   if my_thing
      (x + 2).sin()
   else
      0.0
```
Due to the rules of continuing expressions on other lines, this will be interpreted as `if my_thing(x + 2).sin()`, and not `if my_thing -> (x + 2).sin()`. In this case `->` can be used to separate the condition from the body.

Multiline bodies with `->` can be achieved by using it together with `do`.
```
let s =
   if my_thing -> do
      print("abc")
      (x + 2).sin()
   else
      0.0
```

### `if let`

It is possible to use an optional's presence as a condition for an `if` expression, using the special `if let` construction:
```
let num: ?Int = nil
if let n = num
   print("will not execute")
```
The above code expands to the following:
```
let num: ?Int = nil
if Unwrap.has_value(num)
   let n = Unwrap.unwrap(num)
   do
      print("will not execute")
```

It's possible to chain multiple `if let`s in one if statement, by separating the required values with a comma.

```
if let address = user.home_address, let city = address.city
   print("Hello, " ~ city ~ "!")
```

As can be seen on the example above, subsequent `let` declarations may depend on previous ones. If at least one declaration fails, the entire arm is considered `false` and the next `elif` or `else` arm (or no arm) is executed.

## `match` expressions

`match` allows for a simple form of pattern matching. tsuki's pattern matching is nowhere near as sophisticated as the one found in functional languages, but still provides a reasonable level of ergonomics while keeping the implementation straightforward.

The most basic `match`ing subject is a number:
```
let n = 10
match n
   1 -> print("JEDEN.")
   2..4 ->
      let x = n + 2
      print(x)
   # the matched value can be captured using ||
   5..<10 |value| -> print(value)
   _ -> print("something else")
```

The `_` wildcard pattern matches any value. Patterns are evaluated from top to bottom, so if the wildcard pattern were at the top, all the other branches would be unreachable.

Since the wildcard pattern expects any valid variable name, it can also be used to bind its matched values to variables.

It's also possible to match other primitive subjects, such as Strings or Atoms:
```
let a = :apple
match a
   :orange -> print("The purpose of the columns")
   :apple -> print("One a day keeps the doctor away")
   :banana -> print("slamma")
   _ -> _

let name = "John"
match name
   "Mark" -> print("Oh, hi Mark")
   "Gabe" -> print("Am I really doing the same joke again?")
   n -> print("Hello, " ~ n)
```
Strings and Atoms cannot be used with ranges, because the interaction between the lower and upper bound doesn't make obvious sense.

A more advanced use case for `match` would be unpacking unions. Unions variants carry extra data with them, and union patterns allow for binding the values to variables.
```
union Shape
   Rectangle(Float, Float, Float, Float)
   Circle(Float, Float, Float)

let rect = Shape.Rectangle(32, 32, 64, 64)
match rect
   Shape.Rectangle(x, y, width, height) ->
      print("Rectangle")
      print("X: " ~ x.to_string() ~ "  Y: " ~ y.to_string())
      print(width.to_string() ~ "x" ~ height.to_string())
   Shape.Circle(x, y, radius) ->
      print("Circle")
      print("X: " ~ x.to_string() ~ "  Y: " ~ y.to_string())
      print("radius: " ~ radius.to_string())
```

TODO: Flesh out patterns.

## `?` operator

The `?` operator allows for easy error handling by safely unwrapping a result type. If the result is successful, the `?` operator results in the success value. If the result is an error, the `?` operator returns the error from the current function.

The `?` operator is only allowed in [`try` blocks](#try-expressions). Functions that return results are implicitly wrapped in these blocks.

```
fun fallible(x: Int): !Int
   if x == 0 -> :zero
   elif x == 3 -> :gabe
   else -> x + 4

fun try_example(): !()
   let var x = fallible(2)?
   print(x)  # 6
```

## `try` expressions

`try` expressions enable usage of the `?` operator. By default, all functions that return a result act as if they are wrapped in an implicit `try` block.

Using the `?` operator on an error result in a `try` block will break out of the block, and the block's result value will be the error result.
```
let a = try
   let num_string = stdin.read_line?
   let num = Int32.parse(num_string)?
   num + 2
print(a)
```
In cases where the compiler has trouble figuring out what the error result's type is supposed to be, the `try` can be assigned to a variable with an explicit type specified.
```
let a: IoError!() = try
   _  # do stuff
```

Note that `try` cannot be used as a statement. This has a few effects:
- The result of `try` must be assigned to a variable, or otherwise treated as used (assigned to `_`, used as an argument, etc.)
- The last statement in `try` must always be an expression.

## `const` expressions

`const` expressions can be used to force an expression into being constant.

```
# The value of the variable `x` is known at compile time.
let x = const 4
```
The right-hand side of a `const` expression has the lowest possible precedence. As such, the expression `const 2 + 2` is equivalent to `const (2 + 2)`, and not `(const 2) + 2`.

The return type of a `const` expression is [`const T {V}`](#const-t-v), where `T` is the type of the expression, and `V` is the value of the expression.

Although the subset of types allowed in `const` is quite limited, it can be used for things like compile-time platform detection.
```
if const builtin.target_architecture == :x86_64
   _  # do something x86_64-specific

# Because, `builtin.target_architecture` is compile-time known anyways, the const expression can be
# omitted from the if statement.
if builtin.target_architecture == :x86_64
   _
```
The `if` statements above will both be evaluated at compile-time, and all the `false` branches will be optimized out. This also works with `match`:
```
let os = match const builtin.target_os
   :windows -> "Windows"
   :linux -> "Linux"
   :macos -> "macOS"
   _ -> "exotic OS"

print("Hello, " ~ os ~ " user!")
```

# Statements

## Expression statements

The most basic type of statement in tsuki is an _expression statement_.

An expression statement is made up of a single expression returning `()`. If the expression evaluates to a value of a different type, `let _ = expr` may be used to discard the result.

The most basic example of an expression statement is a `print()`:
```
print("Hello, world!")
```

## Variables and assignment

tsuki distinguishes between two types of variables: immutable variables, and mutable variables.

Variables are introduced into scope using the `let` keyword:
```
let x = 1
```
The left-hand side of the `let` is the pattern that should be used for destructuring the value on
the right. In this case, we simply want to bind the value `1` to a new name `x` so there's no
complicated destructuring involved.

Variables can be read from, but not assigned:
```
print(x)  # 1
# x = 3   # error
```

To create a mutable variable that _can_ be assigned to, `var` can be used in the pattern:
```
let var x = 1
x = 3
print(x)  # 3
```
Note that variables are also statically-typed, so trying to assign a value of a different type than the initial assignment is an error:
```
let var x = 1
x = "Hello"  # error
```

The type of a variable can be specified explicitly by using a `:` after the variable name:
```
let x: Int32 = 1
```

Variable names can be shadowed, as that allows for greater ergonomics. The variable declared last in the innermost scope is always used when resolving shadowed names.

## Scope

A given symbol (resolved identifier) is only accessible in the scope in which it's been declared. The scope of a symbol begins in the block it was declared in, and stretches until the end of that block. The symbol is also visible in all blocks inside of the block it was declared in.

```
let x = 1  # global; visible everywhere
do
   let y = 2  # only visible inside of this `do` block
   do
      let z = 3
      print((x, y, z))
   print((x, y))
print(x)
# print(y)  # error
```

## `if` statement

An `if` statement is functionally and syntactically identical to an `if` expression, except the "last statement must be an expression" rule does not apply.

## `match` statement

Similarly to the `if` statement, `match` statements are functionally and syntactically identical to an `if` expression, but the last statement in `match` arms doesn't have to be an expression.

## `while` loop

A `while` loop runs its block until the provided condition is `false`.
```
let var i = 0
while i < 10
   print(i)
   i += 1
```

Similarly to an `if` statement, `->` may be used to create a single-line loop.
```
let var i = 0
while i < 10 -> i += 1
```

### `while let`

A `while` loop is also capable of iterating over a condition that produces optionals as its result. Enter `while let`:
```
let var bytes = "hello".bytes
while let b = bytes.next
   print(b)
```
The above example is expanded to the following:
```
let var bytes = "hello".bytes
while true
   if let b = bytes.next
      print(b)
   else
      break
```

Similarly to `if let`, `while let` can be chained by separating the `let` declarations with commas.

## `for` loop and iterators

A `for` loop is syntactic sugar over a `while` loop. The syntax of a `for` loop looks like this:
```
for x in iterator
   _
for (x, y, z) in iterator
   _
```
where `x, y, z` are the _loop variables_, and `iterator` is the _iterator_ the loop should use.

The iterator must implement the `Iterator` trait, which is defined like so:
```
trait Iterator[T]
   fun next(self: ^var Self): ?T
```

The `for` loop is then expanded to a regular `while` loop:
```
do
   let var <iterator> = iterator
   while let <variables> = Iterator.next(<iterator>)
      <loop body>
```
Note that all the variables within angle brackets `<>` are not actually visible anywhere, they exist here solely for visualization purposes.

As with all control flow blocks, the single-line `->` notation is supported.
```
for i in (1..10).countup -> print(i)
```

## `break` expression

The `break` expression can be used inside of `while` and `for` loops to jump past the loop, thus, ending the iteration.

The following program will read integers from `stdin` and print them incremented by 1, until something that can't be parsed as a number is entered.
```
import @std.io
import @std.parsing

while true
   write("Enter a number: ")
   let number = stdin.read_line?
   write('\n')
   if let number = Int.parse(number)
      print(number + 1)
   else
      break
```

tsuki does not have a `continue` statement. Wrap your loop body in an `if` instead.

# Functions

Functions are one of the primary building blocks of any program. They are reusable blocks of code, that receive zero or more parameters, and return nothing, or a value.

Functions in tsuki are declared with the `fun` keyword, followed by the function name, a list of parameters, an optional return type, and the function body.
```
# without any parameters, returning ()
fun hello_world()
   print("Hello, world!")

# with a single parameter, returning ()
fun greet(whom: String)
   print("Hello, " ~ whom)

# with a single parameter, returning a String
fun greeting(target: String): String
   "Hello, " ~ target ~ "!"
```

Note how functions return the last expression in their body by default. Every function with a non-`()` return type must return an expression. This is ensured at compile-time by performing control flow analysis over all branches that the function takes.

It is also possible to define multiple parameters of the same type without repeating the type over and over again.
```
fun do_math(a, b, c, d, e, f: Float): Float
   a + b * c - d / e * f
```

tsuki does not allow for defining two distinct functions of the same name within the current scope, aka overloading by parameter types. However, it _does_ have resolution mechanisms for overloaded functions, in case two different modules are imported that declare two functions with the same name.

As already shown in previous examples, calling functions is done using parentheses `()`.
```
hello_world()
greet("world")
print(greeting("world"))
```

## `return` expression

The `return` expression allows for exiting from a function early. In functions returning non-`()` types, `return` must also carry a value to return out of the function.

```
fun find[T](haystack: Seq[T], needle: T): Size
where
   T: Equal

   for (element, i) in haystack.items.enumerate
      if element == needle
         return i
```
The optional expression passed to `return` must be placed on the same line as the `return` keyword, otherwise it's interpreted as a new statement.

The type of `return` is `NoReturn`, because control flow does not go back to the place performing a `return`, so there can be no value there.

# Panicking

Whenever the tsuki runtime encounters an unrecoverable error, it _panics_. A panic aborts the program, unwinds the stack, and prints a stack traceback.

A panic can be triggered using the built-in `panic()` function:
```
let x = 1
if x != 1
   panic("oh my")
```

The `panic()` function's return type is `NoReturn`, because control flow does not return to the calling function.

# Types

tsuki is a statically typed language; this means that every value has an assiociated type with it that's determined at compile-time. Most types are determined from values alone, so it's rarely needed to specify what type a given value is. This section covers all the primitive types (or types with magic syntax) and their properties.

Most "magic" types that use special syntax in type position are available under different names in the standard library, so that they can be used in expression position. An example is accessing the associated functions of a slice:

```
let s = Slice[Int].empty
```

## Unit type `()`

The `()` "empty tuple", or _unit_ type has only one value, which is `()`. Its presence denotes the lack of a useful value, and it's the default return value for functions.

Since there is no runtime cost to creating unit values, the standard library does not include a type name alias for this type.

## `NoReturn`

The `NoReturn` type is used when an expression does not have a return value. An example of this happening is a block with a `return` statement in it; the value the block would normally result in wouldn't be present anyways, hence the block has the `NoReturn` type.

This type can be automatically converted into any other type. Thanks to this property, `return` or `panic` can be used when unwrapping options and results using `or`, for easy error handling:
```
atom Error
   :nil_option

fun some_or_error[T](opt: ?T) !T
   let v = opt or return :nil_option
   v
```

## `Bool`

The `Bool` type has only two possible values: `true` and `false`. Since tsuki is strongly typed, values do not convert into `Bool` automatically.

The `not` operator may be used on a `Bool` to flip its value to the opposite:
```
print(not true)   # false
print(not false)  # true
```

## Integer types

tsuki has a few integer types of different bit widths. Here's a list of all the available integer types:
```
# unsigned integers
Uint8 Uint16 Uint32 Uint64
# signed integers
Int8  Int16  Int32  Int64
```

Integer types of smaller sizes are automatically converted into integer types of bigger sizes (widening conversion), so this works as expected:

```
let x: Int16 = 1_i8
let y: Uint32 = 1_u16
```

Narrowing conversions and conversions between unsigned and signed integers are not performed automatically, and must be done explicitly through the `to_*` family of functions:

```
let x: Uint16 = 1_i8.to_u16 or 0
```

These functions return an optional wrapping the final type, which must be unwrapped using `unwrap` or `or`. The value returned is `nil` if overflow occurred. The `to_*_wrapping` and `to_*_saturating` variants can be used if different behavior is needed.

Arithmetic operators panic on overflow. `add_checked`, `add_wrapping`, `add_saturating`, and friends may be used to protect against panicking.

When choosing a bit size and signedness of an integral type in a typical application, generally stick to `Int32` for "normal" integers; if an integer cannot be negative, eg. an image size needs to be stored, use `Uint32`. Use 64-bit sizes only when needed, such as when storing timestamps.

The `Int` and `Uint` aliases exist as reasonable defaults with a default bit size of 32 bits.

The `Size` alias is an unsigned integer that can be used as a valid size or index into a slice. The size of this type is platform-dependent; on 32-bit machines it's `Uint32`, on 64-bit machines it's `Uint64`.

Smaller bit widths should be avoided whenever possible, unless dealing with a binary format, or conserving bytes for eg. network transmission.

## Floating point types

tsuki has two floating point types: `Float32` and `Float64`. These are signed, IEEE 754-conforming binary32 and binary64 floats.

For user convenience, integer literals get converted to floating point literals whenever possible.

The operators `+`, `-`, `*`, and `/` work as defined in the standard. There's one extra operator, `**`, which is the exponentiation operator. It accepts an integer or floating-point exponent on its right hand side and, for the expression `x ** n`, returns `x` multiplied by itself `n` times. When `n` is a `const` integer, the compiler expands the expression to `n` multiplications without involving a loop.

`impl`s containing extra operations, such as square roots and trigonometry, can be found in the `std.math` module.

The `Float` alias exists as a reasonable default, with a bit size of 32 bits.

## `Atom`

Atoms are a special type, whose value is identified by its name. Atoms are instantiated using the atom literal syntax, described in the Lexis section of this document.

An atom with a given name is always equal to any other atom with the given name:
```
print(:hello == :hello)  # true
print(:hi == :hello)     # false
```

Subsets of atoms may be defined using the `atom` construction:
```
atom Color
   :red
   :green
   :blue
   :yellow
```
The compiler will then ensure that each value of the type `Color` must be one of these four values. This is also known as `enum` in other languages.

To initialize a value from an atom subset when the concrete type is not known, the notation `Subset:atom` can be used.
```
print(Color:red)
```

Atom subsets may define an integer representation explicitly, similar to enums in other languages.
```
atom Key :: int(Uint8)
   0 :a
   1 :b
   2 :c
   # ...
```
The integer representation of a given atom is incremented automatically with each new value, and starts at 0, so the above example can be written simply as:
```
atom Key :: int(Uint8)
   :a, :b, :c
```

Atom subsets may also define a pretty string representation.
```
atom Key
   :a = "A"
   :b = "B"
   :c = "C"
   :shift = "Shift"
   :left = "←"
```
This pretty string representation can be accessed by using the `to_string` function.
```
print(Key:left.to_string)  # ←
```
When an atom is used as an error, this pretty representation is used when printing the error.

The raw name, as declared in the source code, can be obtained by using the `name` function.
```
print(Key:left.name)  # :left
```

Pretty representations can be combined with integer representations.
```
atom Key :: int(Uint8)
   0  :a = "A"
   1  :b = "B"
   2  :c = "C"
   10 :shift = "Shift"
   20 :left = "←"
```

All atoms defined in subsets are _ordered_, which means that they can be compared using the `<` and `<=` operators. Atoms that do not have an explicit integer representation, as well as atoms with an integer representation that do not have holes in them, are also `Ordinal`, which means that they have a preceding and succeeding atom. These can be retrieved by using the `pred` (short for *pred*ecessor) and `succ` (short for for *succ*essor) functions:
```
atom Letters
   :a
   :b
   :c

print(:a.succ)  # Some(:b)
print(:b.succ)  # Some(:c)
print(:c.succ)  # Nil
print(:c.pred)  # Some(:b)
```
For more information on `Ordinal`, refer to the standard library documentation.

## `Char`

`Char` is a type that can store any Unicode codepoint. Internally it's represented by a `Uint32`, however it is distinct from all the integer types, as characters should be treated differently from integers. Values of this type may be obtained through character literals.

## `String`

The `String` type is an immutable UTF-8 encoded string. It can be obtained through one of the available string literal syntaxes described in the Lexis section of this document.

Because all strings are UTF-8, `String` cannot be indexed directly. Instead, one of the available iterators has to be used:
```
let var iter = "cześć".chars
print(iter.next)  # c
print(iter.next)  # z
print(iter.next)  # e
print(iter.next)  # ś
print(iter.next)  # ć

# or, more concisely:
for char in "cześć".chars
   print(char)
```
A `String` can also be used as a byte buffer, by using the `bytes` iterator:
```
let var iter = "\x00\xFF".bytes
print(iter.next)  # 0
print(iter.next)  # 255
```

`String`s can be concatenated using the `~` concatenation operator:
```
print("Hello, " ~ "world!")  # Hello, world!
```

## Optionals

An optional type is a type whose value can be either `nil`, or something meaningful. Values cannot be `nil` unless wrapped in this type.

An optional is written like `?T`, where `T` is the value type. The standard library name for optionals is `Optional[T]`.

As already mentioned, optionals can be initialized to `Nil`:
```
let opt: ?Int = Nil
let correct: ?Int = 3
```

The presence of a value can be queried using the `has_value` function, and the value can later be read using the `unwrap` function.
```
if opt.has_value
   print("That ain't happening, sir")

if correct.has_value
   let ok = correct.unwrap
   print("The unwrapped value is: " ~ ok.to_string)
```

As mentioned previously in the "`if` expressions" section, there exists a shorthand for checking and unwrapping an optional, called `if let`:

```
if let ok = correct
   print("The unwrapped value is: " ~ ok.to_string)
```

The above syntax is sugar for using the previously mentioned `Optional[T]` type. This type is defined like so:
```
pub union Optional[T]
   Some(T)
   Nil

pub type Some[T] = Optional[T].Some
pub type Nil[T] = Optional[T].Nil
```
Thus, optionals can be used with `match` by using the variant names explicitly. The same can be said about constructing optionals.
```
let a = Some(123)
match a
   Some(x) -> print(x)
   Nil -> print("there's no value! :(")
```

## Results

Results are the primary method of handling errors in tsuki. Whenever a function can fail, it can return a result type, whose value is either an error, or a value.

A result type is written like `E!T`, where `E` is the error type, and `T` is the success type. For quick and dirty error handling the shorthand `!T` exists, which is the same as `Atom!T`.

For a given result type `E!T`, both `E` and `T` are implicitly convertible to that result type.

The type `E!T` desugars to the standard library type `Result[E, T]`, which is defined like so:
```
union Result[E, T]
   Error(E)
   Ok(T)
```

`match` can be used with `Result[E, T]` to unwrap errors explicitly:
```
match "123a".parse_int
   Ok(_) -> unreachable()
   Error(error) -> match error
      :overflow -> print("No, that's not it.")
      :invalid_character -> print("Yup, that's right.")
      _ -> _
```

There's also an error handling syntax sugar, which can be found in the [`?` operator](#-operator) section of this document.

## Pointers

Pointers allow for passing _locations_ by reference. They differ from `box` in that unlike an `box`, a pointer is not owned. To maintain memory safety, each pointer has an associated lifetime; this lifetime corresponds to the scope where a pointer's original value originates from. The pointer must not outlive the underlying value, and this is enforced at compile-time. TODO: How?

A mutable pointer type is written as `^var T`. An immutable pointer type is written as `^T`. The standard library aliases for both types of pointers are `PtrVar[T]` and `Ptr[T]`, respectively.

A simple example showcasing mutable pointers would be incrementing an integer variable by passing it by-pointer to a procedure.
```
fun inc(x: ^var Int)
   # Assignment operators automatically dereference pointers on the
   # left-hand side.
   x += 1

let var x = 0
inc(^x)
print(x)  # 1
```
It's also possible to create pointers to other things, such as object fields, and slice elements.

The `^` prefix operator creates a new pointer of the maximum mutability; that is, it favors mutable pointers over immutable ones. However, mutable pointers convert to immutable pointers explicitly, so the operator can be used in all cases.

There can only be a single mutable pointer to a given value, XOR any amount of immutable pointers to that same value. Mutable _and_ immutable pointers to the same value cannot be created, because it could violate lifetime safety in some cases. Consider the case where we take a pointer to some element in a `Seq[T]`, and try to push to the `Seq[T]`: tsuki will not let you do this, because the sequence's inner store could get reallocated if its capacity is exceeded.

Pointers are also subject to automatic dereferencing when calling instance functions, using operators, and performing assignments. Consider this example:
```
object Example
   x: Int

impl Example
   fun print_x(self)
      print(.x)

fun print_x_from(example: ^Example)
   # we can dereference the pointer manually...
   # example^.print_x()
   # ...but we can also let the compiler dereference it for us
   example.print_x()

let var example = Example { x = 1 }
print_x_from(^example)
```

## Ranges

The inclusive and exclusive range operators (`..` and `..<` respectively) are one of the few non-overloadable operators in tsuki. These operators are binary operators, whose both sides must be of the same type, and produce a value of type `Range[T]` and `RangeExcl[T]`.

```
let a = 1..2   # Range[Int]
let b = 1..<3  # RangeExcl[Int]
# The lower and upper bounds can be retrieved using `.lower` and `.upper`.
print(a.lower)  # 1
print(a.upper)  # 2
print(b.lower)  # 1
print(b.upper)  # 3
```

There's also the value `..`, whose type is `RangeFull`.
```
let c = ..  # RangeFull
```

For a `RangeExcl[T]`, if `T` implements `Ordinal`, the exclusive range may be converted to an inclusive range, using the `to_inclusive` function.
```
let excl = 1..<5
let incl = excl.to_inclusive
print(incl)  # 1..4
```
The `to_inclusive` function returns a range with the upper bound replaced with `Ordinal.pred(.upper)`.

## Slices

Slices allow for storing _views_ to arrays of data. They are subject to the same borrow checking rules as pointers.

A slice type is written like `[T]`. A slice type with mutable elements is written like `[var T]`. The slice type in the standard library is `Slice[T]`, and the mutable slice type is `SliceVar[T]`.

Slices are usually initialized from owned `Array[N, T]` or `Seq[T]`.
```
# initialize an Array[3, Int]
let elems = [1, 2, 3]

fun do_stuff(slice: [Int])
   _

do_stuff(elems.slice(..))  # slice all the elements
```
Because the slice was created from a non-`var` `Seq[Int]`, the slice itself is `[Int]`. If the source sequence was `var`, the slice would be `[var Int]`.

We can read elements from this slice by using the `at()` and `get()` methods.
```
print(slice.at(0))  # 1
print(slice.at(1))  # 2
print(slice.at(2))  # 3
# print(slice.at(3))  # error: index out of bounds
print(slice.get(3))  # Nil
```
Out of bounds access with `at()` is checked at runtime and results in a panic. `get()` returns an optional containing the value, if there is a value.

## `Array[N, T]`

`Array[N, T]` is an _owned static_ array type. It's static because its size cannot be changed, and it's allocated on the stack.

Initializing a new array is done using the `[]` prefix:
```
let pi_digits = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
```
Arrays are slices, so the usual `slice()`, `at()`, and `get()` methods can be used on them.
```
print(pi_digits.slice(0..3))  # ^[3, 1, 4, 1]
print(pi_digits.at(0))        # 3
print(pi_digits.get(0))       # Some(3)
print(pi_digits.at(200))      # Nil
```

For a resizable array type, the standard library `Seq[T]` can be used. For an associative array type with arbitrary key types, the standard library `Table[K, V]` can be used.

## `box T`, and `box var T`

`box T` is a shared pointer that allows for multiple ownership of a single value. Usually a value can only have a single owner; with `box T` however a value can be owned by _multiple_ locations.

```
# The `box` operator can be used to create an box value.
let r = box 1
# An box's inner value is immutable by default.
# r = 2  # error: box cannot be implicitly converted to ^var T

# The mutable alternative to box is box var:
let r = box var 1
# Unlike box, its inner value can be modified.
r = 3
print(r^)  # 3
```

Boxes do not share the same lifetime checking limitations as pointers, as their lifetime is automatically extended to be as long as is needed to keep all references to one `box` valid.

```
let var b: ?box Int = Nil
do
   # Perfectly legit, because the box's lifetime is extended.
   let c = box 1
   b = c

# Same here.
fun box_me_an_int(): box Int
   let b = box 1
   b
let i = box_me_an_int()
print(i^)  # 1
```

The exact mechanism used is currently unspecified. TODO: specify

## Tuples

Tuples are values that bind many differently typed values together. A tuple type is specified using the following syntax:
```
type Tup = (A, B, C, ...)
```
where `A, B, C, ...` is a comma-separated list of types to use for the elements of the tuple.
A single-element tuple is specified using the syntax `(A,)`. The zero element tuple `()` is treated specially, as the _unit type_, which only has a single value. The unit type is better described in [its section](#unit-type).

A tuple is instantiated using syntax similar to its type definition, with values in place of types:
```
let elems: (Int, Int, Int) = (1, 2, 3)
```
Individual elements of the tuple can be retrieved through its fields, each of which starts with an underscore `_`, followed by the zero-based index of the field:
```
print(elems._0)
```

A tuple may also be unpacked to separate variables using patterns:
```
let (a, b, c) = (1, 2, 3)
```
Certain elements of a tuple may be ignored while unpacking, using the `_` wildcard pattern:
```
let (x, _, z, _) = (1, 2, 3, 4)
```

## Objects

An object is a named type containing user-defined fields. An object is declared like so:
```
object Example  # the name of the object
   # its fields
   x: Int
   y: Int
   var z, w: Float  # fields can be marked as mutable
```
The object can then be instantiated using the following construction syntax:
```
let e = Example { x = 1, y = 2, z = 3, w = 4 }
```
Normally, all fields must be initialized. However, default expressions can be provided in the object declaration:
```
object Defaults
   var a: Int = 2
   var b: Int = 4
   var c: ?Int = nil
let d = Defaults {}
```
A default value can be any expression. The expressions are evaluated every time the object is constructed. This means that expressions with visible side effects may influence the construction of different object instances:
```
let var counter = 0
fun inc_counter(): Int
   let id = counter
   counter += 1
   id

object Counted
   id: Int = inc_counter()

let a = Counted {}
let b = Counted {}
let c = Counted {}
print((a, b, c))  # Counted { id = 0 }, Counted { id = 1 }, Counted { id = 2 }
```

There's one more exception to the "all fields must be initialized" rule. That is, certain fields may remain uninitialized until they're actually used. These fields still have to be listed in the constructor, but instead of specifying a meaningful value, `uninit` has to be used.
```
object UninitExample
   # note that the field must be var in this case, otherwise we wouldn't be
   # able to set it
   x: Int
   var y: Int

let var u = UninitExample { x = 1, y = uninit }
u.y = u.x + 2
```
It is a compile-time error to try to read from a field that's `uninit`. It is also a compile-time error to try to move an object with `uninit` fields to a different location than where it already is, eg. via `return` or setting a variable.

### Move semantics

Objects in tsuki cannot be copied by default. Instead, they get _moved_ to a new location.

When an object in a location (a variable or object field) is referenced by value, that is, without taking its address using the pointer `^` operator, we say it's _moved_. The original location where the object was stored becomes `uninit` and the new location is the owner of the object. Because the old location becomes `uninit`, it cannot be read from. The moment at which the object is dropped also changes to whenever the new location goes out of scope.

```
object Example
   a: Int32 = 1

let e = Example {}
let f = e
# print(e)  # error: `e` is uninitialized and cannot be read from
```

To enable implicit copying of the object, the `Dup` and `Copy` traits have to be implemented.

`Dup` is a trait that exposes a `dup` function for the object. This function creates a brand new *dup*licate of the object, hence the name `dup`. When `derive`d, the `dup` function will default to simply copy over the existing fields of the object to a brand new instance.

`Copy` is a special trait that the compiler interprets as a marker that this object can be implicitly copied. It also exposes a `copy` function, which, when `derive`d, defaults to simply calling `dup`. Because of this, implementing `Copy` requires `Dup` to be implemented, too.

Both traits are `derive`able, so there's no need to write the boilerplate yourself.
```
object DupExample
   var x, y: Int

impl DupExample
   derive Dup

let a = DupExample { x = 1, y = 2 }
let var b = a.dup()
b.y = 3
print(a)  # DupExample { x = 1, y = 2 }
print(b)  # DupExample { x = 1, y = 3 }

object CopyExample
   var x, y: Int

impl CopyExample
   derive Dup, Copy

let c = CopyExample { x = 1, y = 2 }
let var d = c
d.y = 3
print(c)  # CopyExample { x = 1, y = 2 }
print(d)  # CopyExample { x = 1, y = 3 }
```

### Dropping

When an object instance goes out of scope, we say it's _dropped_. An object implementation may inject some extra code to the dropping procedure via the `Drop` trait.

The trait is defined like so:
```
trait Drop
   fun drop(self: ^var Self)
```

Here's an example implementation of the trait:
```
let var id_count = 0
fun next_id(): Int
   let id = id_count
   id_count += 1
   id

object Dropper
   id: Int = next_id()

impl Drop for Dropper
   fun drop()
      print("Dropping " ~ .id.to_string)

let d = Dropper {}
let e = Dropper {}
let var f = Dropper {}
f = Dropper {}  # Dropping 2
# Dropping 3
# Dropping 1
# Dropping 0
```
The order of dropping objects in variables is from last to first declared variable in a given scope.

## Unions

A union is a type that namespaces a specific set of types, and whose value at runtime may be one of these types. In type system lingo this is known as a _sum type_.

A union is declared using the `union` keyword, followed by the name of the union, continued with a block containing all the possible variant tuples of the union with their types.

```
union Shape
   Rectangle(Float, Float, Float, Float)  # x, y, width, height
   Circle(Float, Float, Float)  # x, y, radius
```

A union variant can be initialized using the union initialization syntax: the union type, followed by the subtype specifying the variant, followed by the variant values in parentheses.
```
let var my_shape = Shape.Rectangle(32, 32, 64, 64)
```

Note that each union variant is a _real type_, and as such, can be specified as the return type from functions, passed around in variables, implemented, and so on.
```
fun make_rectangle(x, y, width, height: Float): Shape.Rectangle
   Shape.Rectangle(x, y, width, height)
```
Each of the union's variant types is a subtype of the union type, so it can be converted to the union type for free.
```
fun print_shape(shape: Shape)
   print(shape)

# The result of make_rectangle (whose type is Shape.Rectangle) is implicitly converted to a Shape.
print_shape(make_rectangle(16, 16, 32, 32))
```
The union type itself implements `AsVariant[T]` for all of its variants.
```
fun obtain_shape(): Shape
   Shape.Circle(16, 16, 16)

let shape = obtain_shape()
let maybe_rectangle = shape.as_variant[Shape.Rectangle]
print(maybe_rectangle)  # None
```
Once a concrete variant is obtained, its fields can be accessed like tuple fields. However, the variant type is _distinct_ from the tuple type with the same fields, and will not convert to it.
```
let rectangle = Shape.Rectangle(16, 16, 32, 32)
print(("width: ", rectangle._2))
```

A union can also have variants that do not contain values. Though, if _no_ variants at all contain values, then it's probably better to use an [atom](#atoms).
```
union MyOption[T]
   Some(T)
   Nil
```

Each of the union's variant types can be brought into global scope by using a type alias. This is done for optionals and results, for more convenient usage.

```
pub type Rectangle = Shape.Rectangle

# in the standard library prelude:
pub type Some[T] = Optional[T].Some
pub type Nil[T] = Optional[T].Nil
pub type Ok[E, T] = Result[E, T].Ok
pub type Error[E, T] = Result[E, T].Error
```

The [`match` expression](#match-expression) can be used to execute code blocks depending on union variants. See its section for more information.

## Type aliases

It's possible to alias types using the `type` declaration. This can be useful when dealing with very long nested generic types.

```
type Strings = Table[String, String]
```

Type aliases defined inside of implementations are called _associated types_, and are described in [Implementations](#implementations-impl).

## Implicit conversion

Certain types can be automatically converted to other types. This process is called _implicit conversion_.

The following implicit conversions are permitted:
- The identity conversion, ie. a type `T` can be implicitly converted to itself.
- Widening conversions for integers that do not change signedness.
   - For example, `Int8` can be converted to `Int16`, but not the other way around.
   - Conversions between integers of different signedness are not permitted, eg. converting from `Uint8` to `Int8` is illegal. This is the case even when the conversion is known to be safe, but this restriction may be relaxed in the future.
- Widening conversions between floating point types.
   - Converting from `Float32` to `Float64` is allowed, but not the other way around.
- Conversions lowering pointer mutability.
   - Converting from `^var T` to `^T` is allowed, but not the other way around.
- Conversions lowering slice mutability.
   - Converting from `[var T]` to `[T]` is allowed, but not the other way around.
- Copying conversions.
   - Converting from `^var T` or `^T` to `T` is allowed only when `T` implements `Copy`.

Implicit conversions are performed in the following places:

- The right-hand side of arithmetic and relational operators is converted to the left-hand side whenever possible.
   - **Note:** This type of conversion will be removed once operator overloading is implemented.
- The right-hand side of assignment is converted to the type of the assignment target whenever possible.
- Function arguments are converted to the expected type whenever possible.

If an implicit conversion fails, a type mismatch error is reported during compilation.

## Implementations (`impl`)

`impl` blocks allow for specifying types and functions associated with types, or instances of types. They also allow for implementing traits.

The following variants of `impl` are available:
```
impl MyObject
impl MyTrait for MyObject
```
The first variant defines functions on `MyObject`, not implementing any traits. The second variant implements the trait `MyTrait` for the `MyObject` type.

Functions with the `self` parameter are referred to as _instance functions_. Types defined on types are referred to as _associated types_.

`impl` blocks can have generic types and constraints attached to them.
```
impl[N, T] Array[N, T]
where
   N: const Size,
   T: Add[T] + Zero

   fun sum(self): T
      let var accumulator = T.zero
      for i in .items()
         accumulator += i
      accumulator

let ints = [1, 2, 3]
print(ints.sum)  # 6
let strings = ["a", "bc", "d"]
# print(ints.sum)  # error: impl's generic constraint is not satisfied
```

`impl` blocks are bound to modules, rather than types - if an `impl` block appears in a module different to where the type was declared, that module has to be imported alongside the module declaring the type for all functions to be available.

Implementing a trait from an external package on a type from an external package is forbidden, as it might break assumptions that the library author wanted to provide the users with. For example, it is not possible to implement `Ordinal` on the standard library `String` type, because a) it wouldn't make sense, and b) the string type is not ordinal, and we shouldn't pretend it is, as all other code operates on that assumption.

It is however possible to augment types from external packages with implementations of locally defined traits, and locally defined methods.

### `Self`

The `Self` type is a special type only accessible in `impl` blocks. This type refers to the type that's currently being `impl`emented.

Note the uppercase letter, though; this type is _not_ to be confused with the `self` variable in instance functions.

`Self` is the only way of referring to associated types.

### The `self` parameter

Functions in `impl` blocks may have a `self` parameter, whose type is automatically deduced based on what the function does with it.

Fields in the current `self` can be accessed using the special `.field_name` notation. Likewise, functions can be called on the current `self` using `.function_name()`.

This notation can also be used to invoke functions on `self`:

```
impl Example
   fun inc_x(self)
      .x += 2

   fun inc_x_twice(self)
      .inc_x()
      .inc_x()
      # syntax sugar for
      # self.inc_x()
      # self.inc_x()
```

By default, what `self` can be is deduced from how the function uses it.
- If the function only ever reads from `self`, it will accept any pointer (`^Self`, `^var Self`, `box Self`, and `box var Self`).
- If the function mutates `self`, it will only accept mutable pointers (`^var Self` and `box var Self`).
- If the function duplicates `self` to an outer location, it will only accept owned pointers (`box Self` and `box var Self`).
- If the function mutates `self` OR duplicates `self` to an outer location whose type is a mutable, owned pointer, it will only accept `box var Self`.

Note that this inference only works for `self` which is a pointer. To move `self` into the function, an explicit type annotation must be used:
```
impl Example
   fun consume(self: Self)
      _  # do something that consumes self
```

If reliance on this inference mechanism is unwanted, an explicit type annotation can also be used with all the pointer types to `Self`.
```
impl Example
   fun explicit_pointer(self: ^Self) -> ()          # accepts ^Self, ^var Self, box Self, box var Self
   fun explicit_pointer_var(self: ^var Self) -> ()  # accepts ^var Self, box var Self
   fun explicit_box(self: box Self) -> ()           # accepts box Self, box var Self
   fun explicit_box_var(self: box var Self) -> ()   # accepts box var Self
```
Do note that these are the _only_ types `self` is allowed to be. Specifying a `self` whose type is not one of the above is an error.

#### Calling instance functions without parentheses

To support pretty getters and setters, in addition to automatically dereferencing pointers on the left-hand side of an assignment, tsuki allows for calling instance functions without parentheses.

```
print(0.sin)
# same as
print(0.sin())
```

The usual way of defining getters and setters is to create a method that returns a `^auto T` pointer to a given field.

```
object Example
   var x: Int
   y: Int

impl Example
   fun x(self): ^auto Int
      ^.x

   # If a field is not `var`, then an immutable pointer has to be used.

   # fun y(self): ^auto Int
   #    ^.y
   # ↑ error: the field 'y' cannot be mutable; make the function return ^Int

   fun y(self): ^Int
      ^.y

let var ex = Example { x = 1, y = 2 }
ex.x = 4
print(ex.y)
# same as the above, but uglier
ex.x() = 4
print(ex.y())
```

### `derive`

The compiler allows for some trait implementations to be _derived_ automatically, only using a type implementation. This can be done using the `derive` declaration inside of an `impl` block.

```
object Copycat
   x: Int

impl Copycat
   derive Dup, Copy
```

## Traits

Traits allow for creating constraints on what functions and associated types a type must implement. A trait is declared using the `trait` keyword.

```
trait Animal
   type Food
   fun speak(self: ^Self)
```
The above code defines a trait `Animal` that requires the implementing type to have a `Food` type and a `speak()` function. Note that the type of `self` must be specified explicitly in traits, because there is not function body, so it's impossible to infer what the pointer's requirements should be. Trait implementations do not have this restriction and will infer the type of the pointer from the trait's definition.

Traits can be used as generic constraints:
```
fun make_it_speak[T](animal: ^T)
where T: Animal
   animal.speak()
```

Traits can also be used like interfaces, where the actual type is determined at runtime, provided they're behind a pointer (be it owned or not).
```
fun make_it_speak_at_runtime(animal: ^Animal)
   animal.speak()
```

Traits can inherit requirements from other traits by using a colon `:` after the trait name.

```
trait DomesticAnimal: Animal
   fun feed(self: ^var Self, food: Self.Food)
```

Implementing `DomesticAnimal` will require that `Animal` is implemented as well.

It's also possible to compose traits in a single constraint, that requires all provided traits to be implemented, using the `+` operator, like `T + U + V`.

Associated types can also be constrained by using the familiar `:` notation.

```
trait AnimalFood
   fun feed(self: ^var Self, animal: ^Animal)

trait Animal
   type Food: AnimalFood
   fun speak(self: ^Self)
```

When instantiating generic traits with associated types, the associated types are provided after generic types, using `K = V` syntax.
```
fun add_two[T](x: T): Int
where T: Add[Int, Ret = Int]
   x + 2
```

### Operator overloading

Several built-in traits exist that allow for overloading existing operators. Here's a list of all the traits, their function names, and the operators they overload:

| Operator | Trait | Function | Meaning |
| -------- | ----- | -------- | ------- |
| unary `not` | `Not` | `unot` | boolean NOT |
| unary `~` | `BitNot` | `bnot` | bitwise NOT |
| unary `-` | `Neg` | `neg` | arithmetic negation |
| binary `+` | `Add` | `add` | arithmetic addition |
| binary `-` | `Sub` | `sub` | arithmetic subtraction|
| binary `*` | `Mul` | `mul` | arithmetic multiplciation |
| binary `/` | `Div` | `div` | arithmetic division |
| binary `**` | `Pow` | `pow` | arithmetic exponentiation |
| binary `<<` | `Lshift` | `lshift` | arithmetic left shift |
| binary `>>` | `Rshift` | `rshift` | arithmetic right shift |
| binary `&` | `BitAnd` | `band` | bitwise AND |
| binary <code>\|</code> | `BitOr` | `bor` | bitwise OR |
| binary `^^` | `BitXor` | `bxor` | bitwise XOR |
| binary `~` | `Concat` | `concat` | concatenation |
| binary `==` | `Equal` | `equal` | value equality |
| binary `<` | `Less` | `less` | ordered relation |
| binary `<=` | `LessEqual` | `less_equal` | |

Other operators are not user-overloadable (such as `and`, `or`, `?`), or derived from other operators (such as `>` and `>=`, derived from `<` and `<=` by flipping the arguments around).

Unary operator traits have a single associated type `Ret`, which signifies the result type of the operator. These traits are defined like so (example based on unary `not`):
```
trait Not
   type Ret
   fun unot(self: ^Self): Self.Ret
```
The only exception here is `Check`, whose `Ret` argument is always `Bool`.

Binary operator traits have a single generic argument `R`, and an associated type `Ret`. `R` specifies the right-hand side of the operation, and `Ret` is the result of the operation. The traits are defined like so (example based on binary `Add`):
```
trait Add[R]
   type Ret
   fun add(self: ^Self, rhs: R): Self.Ret
```

#### The `Unwrap` trait

The `Unwrap` trait is used to overload `if let` declarations. It's defined like so:
```
trait Unwrap
   type Inner
   fun has_value(self: ^Self): bool
   fun unwrap(self: Self): Inner
```

### Calling functions from specific traits

Sometimes, two functions from different traits can share the same name. In these cases the method can be qualified explicitly, as a member of the trait.

```
trait ExampleA
   fun my_method(self: ^Self)

trait ExampleB
   fun my_method(self: ^Self)

object Example

impl ExampleA for Example
   fun my_method(self)
      print("Called on ExampleA")

impl ExampleB for Example
   fun my_method(self)
      print("Called on ExampleB")

let e = Example {}
# e.my_method()  # error: ambiguous call; resolves to more than one function
ExampleA.my_method(e)  # called on ExampleA
ExampleB.my_method(e)  # called on ExampleB
```

## `const T {V}`

`const T {V}` is a special class of types that represents values known at compile-time. Each literal that is supported as `const` starts out with the type `const T {V}`, where `T` is the type of the literal, and `V` is the literal's value. A value whose type is `const T {V}` implicitly converts to `T`, without any runtime cost.

Currently supported types for `T` include:
- `Bool`
- `Int`
- `Index`
- atoms (`Atom` and user-defined sets)

While `const T {V}` is always a concrete type, `const T` on the other hand is a trait that all types from the `const T {V}` class satisfy.

It might be hard to grasp the concept just by looking at these abstract words, so let's consider an example: creating a type-safe, two-dimensional `Array[N, T]`.

```
object Array2D[W, H, T]
where
   W: const Size,
   H: const Size

   var inner: Self.Inner

impl[W, H, T] Array2D[W, H, T]
where T: Dup
   type Inner = Array[W * H, T]

   fun new(default_value: T): Self
      Self { inner = Inner.filled_with(default_value) }

impl[W, H, T] Array2D[W, H, T]
   fun bounds_check(position: (Size, Size))
      assert(position._0 < W and position._1 < H, ("index out of bounds: ", position))

   fun flat_index(position: (Size, Size)): Size
      position._0 + position._1 * W

   fun at(self, x, y: Size): ^auto Ret
      let position = (x, y)
      Self.bounds_check((position))
      .inner.at(Self.flat_index(position))

let var image = Array2D[10, 10, Uint8].new(0)
print(image.at(0, 0))  # 0
image.at(5, 5) = 255
```

# Generics

tsuki allows for adding types as parameters to functions, objects, unions, and traits. These parameters are known as _generic parameters_.

Whenever one uses a generic symbol, the symbol gets _instantiated_ with the provided arguments - this means that a new copy of the symbol is created, with all references to the generic parameters replaced by the provided arguments. These _generic instantiations_ are cached within the original type, so eg. using a `Seq[Int]` twice results in the same type.

Generic parameters are always placed after the symbol name. Examples include:
```
fun do_stuff[T]()
object MyContainer[T]
union MyUnion[T]
trait MyTrait[T]
```

Normally, generic parameters can accept any type. For the sake of early type checking and error messages, such unconstrained generic types are pretty much opaque to the symbol that uses them; that is, no methods or operators are callable on the generic parameter in the symbol's implementation.

To allow for calling functions on these symbols, the generic type has to be constrained using traits. Generic constraints are specified after the declaration, before pragmas, using the `where` keyword:
```
fun add_two[T](x: T): T.Ret
where T: Add[Int]
   x + 2
```

Calling a generic function or using a generic object with explicit types uses the same syntax as generic parameters.
```
print(add_two[Int64](2))  # 4
let c = MyContainer[Int] {}
```

`impl` blocks can have generic parameters. Note that the parameters are specified after the `impl` keyword, as implementations don't define any symbols on their own.
```
impl[T] Seq[T]
   _
```
These generic parameters are used to provide generic _arguments_ to types. Constraints for generic parameters in `impl` blocks are inferred automatically, so it's only required to specify a master set of constraints wherever the implemented type is declared:
```
object Vec2[T]
where
   T: Default

   var x, y: T

# Here, the T is inferred to have the Default constraint.
impl[T] Vec2[T]
   fn zero(): Self
      Self {
         x = T.default(),
         y = T.default(),
      }
```
Additional constraints can be specified after the `impl` block's implemented type. This can be used to only enable certain sets of functions when a generic parameter implements some trait:
```
impl[T] Add[Self] for Vec2[T]
where
   T: Add[T]

   type Result = Self

   fun add(self, rhs: T): Self
      Self {
         x = self.x + rhs.x,
         y = self.y + rhs.y,
      }
```
In the example above, `Vec2[T]` will only overload the `+` operator if `T` implements the `Add[T]` trait.

# Modules

tsuki programs are always split into modules. Compilation is performed by telling the compiler which module is the main module, and the compiler will recursively compile every module imported into the program.

In tsuki, a single file always represents a single module. Modules can _import_ code from other modules by using the `import` statement.

File: b.tsu
```
fun add_two(a: Int): Int
   a + 2
```
File: a.tsu
```
import b

print(add_two(2))  # 4
```

Compiling and running the program `a.tsu` will also result in the module `b.tsu` getting compiled.

The `import` statement imports _all_ symbols from a given module into the current scope. Symbols can still be fully qualified using the module name, eg. in the example above, `add_two(2)` and `b.add_two(2)` would do the same thing.

It's possible to only import a limited set of symbols into the scope:
```
import @std.io for stdin, stdout
```
It's also possible to not import _any_ symbols into the scope, and instead force full qualification:
```
import @std.io for _
```
This is not recommended however, as having to fully qualify each module name results in ugly code. Unlike Python, tsuki can usually deal with cases where two symbol names conflict with each other, and in cases where it can't, the compiler will throw an error when trying to use such conflicting symbols.

The `import` statement also allows for renaming the module name for convenience:
```
import unbearably_long_module_name as ulmn
```
This can be combined with importing a limited set of symbols; the `for` has to go _after_ the `as`:
```
import unbearably_long_module_name as ulmn for Thingy
```

To tell the compiler that an import should be performed from within an external package, `import @name` must be used. This includes imports from the standard library, because it's treated as an external package.

```
import @std.testing
```

## Packages

Apart from modules, tsuki also has the concept of _packages_. A package is a collection of related modules. The standard package structure is like so:
```
package
└── src
    ├── source.tsu
    ├── files.tsu
    ├── go.tsu
    └── here.tsu
```

The `src` folder is known as the _source root_ of the package. All local imports are resolved from the source root of the package: if the module `files.tsu` wants to import `go.tsu`, it must `import go`.

It's also possible to import from subdirectories, by using slashes `/` to separate paths. Let's take the following package as an example:
```
package
└── src
    ├── main.tsu
    └── tools
         └── utility.tsu
```

The file `main.tsu` wants to import the file `tools/utility.tsu`. To do that, the following import statement can be used:

File: main.tsu
```
import tools.utility
```

An external package manager may instruct the compiler to treat a directory as an external package. By default, the directory containing the source code of the standard library, `std`, is such a package.

Modules from external packages can be imported using a dot `.`:
```
import @std.os
```

If a package isn't necessarily split up into smaller modules, like `std` is, then it may have a _main module_. The main module's name must be the same as the package's name:
```
package
└── src
    └── package.tsu
```
Then, other packages can import the package without specifying which submodule to import:
```
import package
```

## Cross-package visibility

By default, all top-level symbols are visible _only to other modules within the same package_. Packages cannot access symbols from external packages, unless their declarations are marked with the `pub` keyword.

Package: adder, File: src/adder.tsu
```
# this function isn't marked public, so it's not accessible from
# other packages
fun adding_abstraction(a, b: Int): Int
   a + b

# objects can be marked public
pub object Adder
   # object fields can also be marked public
   # the `pub` marker sits _before_ `var`
   pub increment: Int

impl Adder
   # functions inside of `impl` can also marked public
   pub fun add(self, to: Int): Int
      adding_abstraction(to, .increment)

   pub fun init(increment: Int): Self
      Self { increment = increment }
```

Package: my_program, File: src/my_program.tsu
```
import @adder

let two = Adder.init(2)
print(two.add(2))  # 4
```

It's also possible to reexport some imported symbols, by using `pub` before an `import` statement.
```
pub import @std.math
```

## Conditional compilation

tsuki has a very limited model for conditional compilation, but one that's enough for most use cases.

The only form of conditional compilation available, is conditionally importing a module depending on the target OS.

Which OS a given module is built for, is specified by appending `.<os name>` to the filename before the `.tsu` extension, eg. `example.windows.tsu`. All OS-specific modules are semantically checked during compilation, so if you have eg. `example.windows.tsu` and `example.linux.tsu`, _both_ will be checked, but only the one matching the target OS will be compiled to the final executable.

## Package management

tsuki uses a fairly novel way of dependency management. Instead of the traditional package manager giving the compiler paths to packages, instead _the compiler_ asks the package manager where all the dependencies can be found.

Dependencies are specified using the `dependency` keyword, and must be specified in the main file of the package.

### Adding dependencies

A dependency declaration is made up of the URL, and optionally `as some_name` specifying what the `import @` namespace should be:

```
# Use a package from the Lunar Base package registry:
dependency "lunar:liquidev/gmath/0.1.0"
# Clone a git package and rename its namespace:
dependency "git://github.com/liquidev/tsukipng?tag=0.1.0" as png
```

The compiler itself does not validate and interpret the URL. That task is on behalf of the package manager.

#### Compiler-package manager IPC model

Negotation between the compiler and the package manager as to where packages can be found is done simply via the compiler outputting the names of packages it needs to stdout, and reading the package names and paths from stdin.

The protocol used for communicating between the package manager and the compiler is fairly simple. The compiler simply sends the dependency URLs to the package manager, line by line. The package manager is expected to respond to each line with a package namespace, which must be a valid identifier, and the path to the package's `src` directory, separated by a tab (ASCII 09h).

Example input:
```
lunar:liquidev/gmath/0.1.0
git://github.com/liquidev/tsukipng?tag=0.1.0
```
Example output:
```
gmath	/home/liquid/.packages/lunar/liquidev/gmath/0.1.0/src
tsukipng	/home/liquid/.packages/git/github.com/liquidev/tsukipng/tags/0.1.0/src
```

# Documentation

Documentation in tsuki is similar to that in Rust, with a flavor of Javadoc. Doc comments may appear before symbol declarations, and use GitHub Flavored Markdown for text. Let's take the previous example with the `Adder` object and document it:
```
fun adding_abstraction(a, b: Int): Int
   a + b

## An adder adds a constant increment to a provided value.
pub object Adder
   ## The increment of the adder. This is the value added to other values, when `add` is called.
   pub increment: Int

impl Adder
   ## Adds the increment the adder was initialized with to `to`.
   ##
   ## @param to  The value to increment.
   ## @return    The incremented value.
   ##
   ## @see Adder.init
   pub fun add(self, to: Int): Int
      adding_abstraction(to, .increment)

   ## Initializes a new adder with the given increment and returns it.
   ##
   ## @param increment  The increment to initialize the adder with.
   ## @return           The freshly initialized adder.
   ##
   ## @see Adder#add
   pub fun init(increment: Int): Self
      Self { increment = increment }
```

The above example showcases a few features of the documentation system. Let's dissect a single doc comment and analyze it:

```
## Initializes a new adder with the given increment and returns it.
```
The first paragraph in a doc comment is the _brief_ of a given symbol. The purpose of this paragraph is to give a brief overview of what the symbol's purpose is.

The brief ends with an empty line, separating it from the _description_. If a function is self-explanatory enough, a description usually isn't included, like in our case.

Next up are the generator directives. These tell the documentation generator what extra elements of documentation should be added to the generated page.

```
## @param increment  The increment to initialize the adder with.
## @return  The freshly initialized adder.
```
The `@param` and `@return` directives tell the generator to include descriptions of parameters and the return type.

```
## @see Adder#add
```
The `@see` directive tells the generator to add a cross-reference to the given function. By convention, the `Type.function` notation is used for type-bound functions, and the `Type#function` is used for instance functions.

Here's a list of all supported directives:

- `@param` - specifies an extra description for a parameter.
- `@generic` - specifies an extra description for a generic parameter.
- `@return` - specifies an extra description for the return value.
- `@see` - specifies related symbols.
- `@deprecated` - specifies that the given symbol is deprecated, optionally with a message on what should be used instead.
- `@unsafe` - displays a warning that the symbol is unsafe and extra care should be taken when using it.

More directives may be added in the future.

# Pragmas

Pragmas are tsuki's way of specifying extra hints to the compiler. They are always placed after a `::` following the full declaration of a symbol, but before its definition. Examples of how to apply pragmas:
```
fun abc() :: some_pragma
# Pragmas can also be wrapped around to the next line for readability.
fun abc()
:: some_pragma

object MyObject :: some_pragma

atom Things :: some_pragma
atom Pink in Color :: some_pragma

# There are currently no pragmas for unions, so the syntax is not supported.
```
Various pragmas that can be applied in specific places are mentioned throughout this manual.

# Macros

TODO: still to be discussed with others, need some opinions on how a _good_ macro system should work.
Maybe tsuki should not have macros after all? My opinion is that macros should operate on a limited subset of the AST, such that AST breakage should almost never affect them.
What about syntax and autocomplete support? Maybe two different kinds of macros should be exposed, like Rust's `macro_rules!` and `proc_macro`s?
Also, would inline iterators be considered macros, or implemented as macros, or...?

# C FFI

To facilitate interfacing with libraries written in other programming languages, tsuki sports a foreign function system for importing and exporting functionality, from and to C.

## C integer types

tsuki exposes a set of integer types that correspond to platform-specific C types.

| tsuki type | C type |
| --- | --- |
| `CChar` | `char` |
| `CUChar` | `unsigned char` |
| `CShort` | `short` |
| `CUShort` | `unsigned short` |
| `CInt` | `int` |
| `CUint` | `unsigned int` |
| `CLong` | `long` |
| `CULong` | `unsigned long` |
| `CLongLong` | `long long` |
| `CULongLong` | `unsigned long long` |

## `RawPtr[T]`, `RawPtrVar[T]`, `RawPtrSlice[T]`, and `RawPtrVarSlice[T]`

The `RawPtr` type is used for creating and manipulating unmanaged pointers to data. Though it looks like a normal type, it's actually a magic type implemented in the compiler itself. `RawPtr[T]` acts similarly to `^T`, and `RawPtrVar[T]` acts similar to `^var T`, but their lifetime is not managed by the compiler. They can be created freely by using their `to` functions, and they can be dereferenced just like regular pointers:

```
let var x = 1
let var p = RawPtr.to(^x)
print(p^)  # 1
```

`RawPtr.to`, `RawPtrVar.to`, `RawPtrSlice.to`, and `RawPtrVarSlice.to` take a (non-`var` or `var`) pointer as an argument, and convert it to the appropriate unmanaged pointer. `Var` unmanaged pointers require `^var T` pointers.

Reading from an unmanaged pointer that points to invalid memory (aka dangling pointer) is undefined behavior.
```
let var p: RawPtr[Int]
do
   let var a = 1
   p = RawPtr.to(^a)
# print(p^)  # undefined behavior, because `a` doesn't exist anymore!!!
```

`RawPtrSlice[T]` can be used to create pointers to slices. Its `to` function accepts a slice whose elements the pointer should point to. Note that `RawPtrSlice` is not dereferenced using the usual `^` operator, but rather the index operator.
```
let var s = [1, 2, 3]
let var a = RawPtrSlice.to(s.slice(..))
print(a[1])  # 2
```
`RawPtrSlice`s do not store the length alongside the pointer like slices do, so using them allows for unbounded access, which is undefined behavior.

`CString` is available as an alias to `RawPtrSlice[CChar]`.

Idiomatic tsuki programs should avoid using these unmanaged pointers whenever possible. When interfacing with C, idiomatic wrappers should be provided if time allows, such that developers do not need to think about managing lifetimes.

Unmanaged pointers can be used for optimization purposes if necessary, but should be avoided like the fire wherever better, more robust solutions exist.

## Importing C functions

Functions can be imported from C by using the `c_import` pragma. This pragma accepts a string as a paramter, specifying what the C function is called.

```c
int hello_ffi(int x)
{
   return x + 2;
}
```
```
fun hello_ffi(x: CInt) :: c_import("hello_ffi")
```
Functions imported from C must not have a body.

### Variadics

Variadic functions like `printf` can be imported by using `c_varargs`.

```
fun c_printf(fmt: CString): CInt :: c_import("printf"), c_varargs

c_printf("Hello, %s", "world")
```

Note that functions using C-like varargs cannot be created from tsuki, only imported from C. Other, safer forms of accepting a variable number of arguments should be preferred, such as accepting a length + `RawPtrSlice[T]` pair.

## Exporting functions to C

Functions can also be exported to C by using the `c_export` pragma. Similarly to `c_import`, it accepts a string that specifies the function name.
```
fun add_three(a, b, c: CInt): CInt :: c_export("add_three")
   a + b + c
```
```c
#include <stdio.h>

int add_three(int a, int b, int c);

int main(void)
{
   printf("%i\n", add_three(1, 2, 3));
   return 0;
}
```

## C structs

tsuki allows for defining objects that are compatible with the C ABI. For that, the `c_struct` pragma can be used.
```
object Things :: c_struct
   a, b, c: Int

impl Things
   # C structs can also have associated and instance functions.
   fun init(): Self :: c_import("things_init")
   fun increment(self: ^var Self) :: c_import("things_increment")
```
Instance functions imported from C with `self: ^var Self` are assumed to accept `Self *` as the first argument, functions with `self: ^Self` are assumed to accept `const Self *` as the first argument, and functions with `self: Self` are assumed to accept `Self` as the first argument. Note that in case of functions imported from C the type of `self` must be specified explicitly, as there is no body to infer it from.
```c
struct things {
   int a, b, c;
};

struct things things_init();
void things_increment(struct things *);
```

## C unions

tsuki unions are never ABI-compatible with C unions. However, tsuki exposes a mechanism for declaring C unions, via the `c_union` pragma attachable to objects.
```
object Caster :: c_union
   var x: Float32
   var y: Int32
```
Uninitialized fields from C unions can always be read from, to mimic C bitcasting behavior. In most cases the `bitcast` function from the standard library should be used for the purposes of bitcasting, rather than dealing with C unions directly.

## Compiling and linking external object files and libraries

tsuki does not expose a way of linking extra object files with the program from within the language itself. These must be done by the compiler, using the `--link-object` and `--link-library` flags. These flags are usually managed by a build system or package manager.

tsuki also does not expose a way of compiling C objects. Again, running the C compiler must be done by an external build system.
