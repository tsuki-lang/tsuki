# tsuki

My idea for a programming language that's actually fun to program in.

---

# Lexis

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
_ and atom catch derive do for fun if impl in is macro match not object of or
pub return try type union while val var
```

The identifier `_` is special; it's an identifier that is used for ignoring things. Variables with the name `_` cannot be read from, and the identifier cannot be used as a valid function or object name. Additionally, when used as a statement, it's a no-op, and can be used to create empty blocks.

## Blocks

tsuki makes use of significant indentation for program structure. Unlike most programming languages, the indentation level is stored *per token*, which not only simplifies the implementation, but also avoids ugly backslash escapes for continuing lines, and yields more meaningful error messages.

Each leading whitespace character ` ` adds 1 to the indentation level of tokens on a given line. Tabs are not permitted, as they do not render with a consistent width across different text editors, thus causing confusion.

The most simple block is the _do-block_:
```
do
  _ # statements go here
```

The special identifier `_` may be used when a dummy no-op statement is needed, ala `pass` from Python.

The standard indent size is 2 spaces, but a different amout may be used depending on the developer's preferences.

## Operators

tsuki defines a few standard operators.

The following prefix operators are available:
```
not ~ - () [] {} . ^ ..
```

The following infix operators are available. The list is sorted by precedence, where top = biggest precedence, and bottom = lowest precedence. Lines with more than one operator contain operators of equal precedence.
```
() {} [] . ^ ? !
**
* / << >> & | ^^
+ - ~
.. ..=
== != < > <= >= is of in
and
or
= += -= *= /= <-
```

Most of the listed operators are overloadable, see [Operator overloading](#operator-overloading).

The token `->` appears inside the syntax, but is _not_ an operator - it's only used for punctuation. This token is the "then" arrow, used in `if` and `match` expressions to separate the condition from a single-line expression body.

# Parsing

tsuki programs are read in lines from top to bottom, left to right, until the end of a file is reached.

Top-level declarations, as well as declarations within `impl` blocks, are order-independent. Symbols from below can access symbols from above, and vice versa. This does not apply to bodies of functions, where variables declared later in scope cannot be accessed before their declaration.

Similar to scripting languages like Python, regular code may appear outside of a function. This is known as _top-level_ or _module level_ code. Top-level code in a single module is evaluated in the usual order (top to bottom, left to right). Code from other modules is evaluated in the order they're imported from that module, and top-level code in a single module is only evaluated once.

# Expressions

Expressions in tsuki are made up of two types of operations: _prefix_, and _infix_. _prefixes_ begin an expression, and _infixes_ continue an already existing expression. Consider this very simple example:

```
1 + 2
```
An expression starts with a prefix, and literals are prefixes, so `1` is read. Then, the next token is examined, to determine whether it's a valid infix - in our case `+` is a valid infix, so parsing continues in the `+` rule, which expects another expression on its right-hand side.

This is also where precedence comes in: if it's part of a statement, an expression is parsed with the precedence 0, which is lower than any other precedence level. The precedence level determines which infix operators are treated as part of the current expression; if an operator's precedence level is lower than that of the current expression, it cannot be part of this expression, so it's skipped, and later picked up by an expression with a lower precedence level. This algorithm is known as *Pratt parsing*, or *precedence climbing*.

Infix tokens that continue a line must have an indentation level greater than the first token of the expression:
```
val test = "hello" ~ " world "
  ~ "this" ~ " is " ~ "a "
  ~ "test"
# or
val test = "hello" ~ " world " ~
  "this" ~ " is " ~ "a " ~
  "test"
```

## Prefixes

Prefixes include:
- literals (nil, booleans, integers, floats, atoms, strings)
- identifiers
- prefix operators
- grouping/tuple construction `()`
- sequence literals `[]`
- table literals `{}`
- `do`, `if`, and `match` expressions
- `try` expressions

## Infixes

Infixes include:
- infix operators
- function calls `()`
- indexing operators `[]` and `{}`
- `catch` expressions

## `or` and `and` operators

The `or` and `and` operators are special, non-overloadable operators. They are not overloadable because they perform _short-circuit evaluation_. This means that when the first operand already determines the result of the operation, the second operand will not be evaluated.

In case of `or`, if the first operand is `true`, the second operand is not evaluated, and `true` is returned.

In case of `and`, if the first operand is `false`, the second operand is not evaluated, and `false` is returned.

The `or` operator can be used to safely unwrap an optional or a result while providing a default value, in case a value is not present, or an error occured. For results, the error is discarded.
```
val nope: ?Int = nil
val yes = nope or 1
print(yes)  # 1
```
Following the rule of short-circuit evaluation, the second operand is not evaluated if the value is unwrapped successfully.

Analogic behavior for `and` does not currently exist.

## `is` operator

The `is` operator may be used to check whether a certain value is of a given type, or whether two different type aliases resolve to the same type.

```
val a = 1
assert(a is Int)

type MyInt = Int
assert(MyInt is Int)
```

## `of` operator

The `of` operator may be used to check whether a given object is of a given type _at runtime_.

```
object Parent of Root
object Child of Parent

val c = Child {}
val p: Parent = c
assert(p2 is Parent)
assert(not (p2 is Child))
assert(p2 of Child)
```

## `do` expressions

The `do` expression evaluates a block of code and returns the result of the last statement in the block.

```
print(do
  val x = 1
  x + 2
)  # 3
```

Note that the last statement *must* be an expression statement, as the `do` expression must have a result.

## `if` expressions

The `if..elif..else` expression evaluates a boolean condition, and if it's `true`, evaluates the block of code following that condition. If the condition is `false`, the next condition is evaluated, and so on. If none of the conditions evaluate to `true`, the `else` arm is executed.

```
import std.io

val name = io.stdin.read_line()
print(
  if name == "Mark"
    "Oh hi Mark"
  elif name == "Gabe"
    "How's that 3rd game going?"
  else
    "Hello, " ~ name ~ "!"
)
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

### `if val`

It is possible to use an optional's presence as a condition for an `if` expression, using the special `if val` construction:
```
val num: ?Int = nil
if val n = num
  print("will not execute")
```
The above code expands to the following:
```
val num: ?Int = nil
if num?
  val n = num!
  do
    print("will not execute")
```

It's possible to chain multiple `if val`s in one if statement, by separating the required values with a comma.

```
if val address = user.home_address, val city = address.city
  print("Hello, " ~ city ~ "!")
```

As can be seen on the example above, subsequent `val` declarations may depend on previous ones. If at least one declaration fails, the entire arm is considered `false` and the next `elif` or `else` arm (or no arm) is executed.

## `match` expressions

`match` allows for a simple form of pattern matching. tsuki's pattern matching is nowhere near as sophisticated as the one found in functional languages, but still provides a reasonable level of ergonomics while keeping the implementation straightforward.

The most basic `match`ing subject is a number:
```
val n = 10
match n
  1 -> print("JEDEN.")
  2..=4 ->  # 2..=4 is an inclusive range
    val x = n + 2
    print(x)
  # the matched value can be captured using ||
  5..10 |val| -> print(val)
  else -> print("something else")
```

It's also possible to match other primitive subjects, such as Strings or Atoms:
```
val a = :apple
match a
  :orange -> print("The purpose of the columns")
  :apple -> print("One a day keeps the doctor away")
  :banana -> print("slamma")
  else -> _

val name = "John"
match name
  "Mark" -> print("Oh, hi Mark")
  "Gabe" -> print("Am I really doing the same joke again?")
  else |n| -> print("Hello, " ~ n)
```
Strings and Atoms cannot be used with ranges.

`match` combined with `catch` also allows for matching errors:
```
atom Error
  :oops
  :not_found

val err: !Int = :oops
val int = err catch |err_atom|
  print(
    match err_atom
      :not_found -> "404"
      :oops -> "Somebody's gonna get fired"
      else |ok| -> "Everything's fine: " ~ ok
  )
  return
```

A more advanced use case for `match` would be unpacking unions. Unions carry extra data with them, and this data can be captured using `||`, just like shown previously with `else` and ranges.
```
union Shape
  :rectangle(Float, Float, Float, Float)
  :circle(Float, Float, Float)

val rect = Shape:rectangle((32, 32, 64, 64))
match rect
  :rectangle(x, y, width, height) ->
    print("Rectangle")
    print("X: " ~ x.to_string ~ "  Y: " ~ y.to_string)
    print(width.to_string ~ "x" ~ height.to_string)
  :circle(x, y, radius) ->
    print("Circle")
    print("X: " ~ x.to_string ~ "  Y: " ~ y.to_string)
    print("radius: " ~ radius.to_string)
```
`match` is the only way of retrieving the variant data from unions.

## `try` expressions

`try` expressions allow for easy error handling by safely unwrapping a result type. If the result is successful, the `try` expression results in the success value. If the result is an error, the `try` expression returns the error from the current function.

The current function must have a compatible result as its return type for this to work.

```
fun fallible(x: Int): !Int
  if x == 0 -> :zero
  elif x == 3 -> :gabe
  else -> x + 4

fun try_example(): !()
  var x = try fallible(2)
  print(x)  # 6
```

## `catch` expressions

As an alternative to `try`, it's possible to unwrap an error while still preserving the error value using the `catch` expression. Building on the above example, it's possible to handle the two errors separately by `catch`ing the error:

```
fun catch_example()
  var x = fallible(2) catch |err|
    if err == :zero -> print("Zero was passed to fallible()")
    elif err == :gabe -> print("Gabe isn't gonna be very happy about this")
    return
```

Note how in this case the function doesn't need to return a result type. This is because we _catch_ the result instead of bubbling it up the function call stack.

The `catch` block must return a default value for the resulting expression, so as not to leave a value uninitialized. As per the usual rules, `NoReturn` is also allowed, as it implicitly converts to any other type.

# Statements

## Expression statements

The most basic type of statement in tsuki is an _expression statement_.

An expression statement is made up of a single expression returning `Void`. If the expression evaluates to a value of a different type, `val _ = expr` may be used to discard the result.

The most basic example of an expression statement is a `print()`:
```
print("Hello, world!")
```

## Variables and assignment

tsuki distinguishes between two types of variables: immutable variables, and mutable variables.

An immutable variable is created using the `val` keyword:
```
val x = 1
```
The variable can be read from, but not assigned to:
```
print(x)  # 1
# x = 3   # error
```

To create a mutable variable, `var` can be used:
```
var x = 1
x = 3
print(x)  # 3
```
Note that variables are also statically-typed, so trying to assign a value of a different type at this point is an error:
```
var x = 1
x = "Hello"  # error
```

The type of a variable can be specified explicitly by using a `:` after the variable name:
```
val x: Int32 = 1_i32
```
Multiple variables of the same type and value may be declared this way, by simply specifying more than one name separated by a comma:
```
val x, y, z = 2
```
If the value should be different, a separate variable declaration should be used.

Variable names can be shadowed, as that allows for greater ergonomics. The variable declared last in the innermost block is always used when resolving shadowed names.

## Scope

A given symbol (resolved identifier) is only accessible in the scope in which it's been declared. The scope of a symbol begins in the block it was declared in, and stretches until the end of that block. The symbol is also visible in all blocks inside of the block it was declared in.

```
val x = 1  # global; visible everywhere
do
  val y = 2  # only visible inside of this `do` block
  do
    val z = 3
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
var i = 0
while i < 10
  print(i)
  i += 1
```

### `while val`

A `while` loop is also capable of iterating over a condition that produces optionals as its result. Enter `while val`:
```
var bytes = "hello".bytes
while val b = bytes.next
  print(b)
```
The above example is expanded to the following:
```
var bytes = "hello".bytes
while true
  if val b = bytes.next
    print(b)
  else
    break
```

Similarly to `if val`, `while val` can be chained by separating the `val` declarations with commas.

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
  fun var next(): ?T
```

The `for` loop is then expanded to a regular `while` loop:
```
do
  var <iterator> = iterator
  while val <variables> = <iterator>.next
    <loop body>
```
Note that all the variables within angle brackets `<>` are not actually visible anywhere, they exist here solely for visualization purposes.

## `break` statement

The `break` statement can be used inside of `while` and `for` loops to jump past the loop, thus, ending the iteration.

The following program will read integers from `stdin` and print them incremented by 1, until something that can't be parsed as a number is entered.
```
import std.io
import std.parsing

while true
  write("Enter a number: ")
  val number_string = stdin.read_line
  write('\n')
  if val number = number_string.parse_int
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

The `return` expression allows for exiting from a function early. In functions returning non-`()`  types, `return` must also carry a value to return out of the function.

```
fun find[T: Equal](haystack: Seq[T], needle: T): Index
  for i, element in haystack.items
    if element == needle
      return i
```
The type of `return` is `NoReturn`, because control flow does not go back to the place performing a `return`, so there can be no value there.

# Panicking

Whenever the tsuki runtime encounters an unrecoverable error, it _panics_. A panic aborts the program, unwinds the stack, and prints a stack traceback.

A panic can be triggered using the built-in `panic()` function:
```
val x = 1
if x != 1
  panic("oh my")
```

The `panic()` function's return type is `NoReturn`, because control flow does not return to the calling function.

# Types

tsuki is a statically typed language; this means that every value has an assiociated type with it that's determined at compile-time. Most types are determined from values alone, so it's rarely needed to specify what type a given value is. This section covers all the primitive types (or types with magic syntax) and their properties.

## Unit type `()`

The `()` "empty tuple", or _unit_ type has only one value, which is `()`. Its presence denotes the lack of a useful value, and it's the default return value for functions.

## `NoReturn`

The `NoReturn` type is used when an expression does not have a return value. An example of this happening is a block with a `return` statement in it; the value the block would normally result in wouldn't be present anyways, hence the block has the `NoReturn` type.

This type can be automatically converted into any other type. Thanks to this property, `return` or `panic` can be used when unwrapping options and results using `or`, for easy error handling:
```
atom Error
  :nil_option

fun some_or_error[T](opt: ?T) !T
  val v = opt or return :nil_option
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
val x: Int16 = 1_i8
val y: Uint32 = 1_u16
```

Narrowing conversions and conversions between unsigned and signed integers are not performed automatically, and must be done explicitly through the `to_*` family of functions:

```
val x: Uint16 = 1_i8.to_u16 or 0
```

These functions return an optional wrapping the final type, which must be unwrapped using `!` or `or`. The value returned is `nil` if overflow occurred. The `to_*_wrapping` and `to_*_saturating` variants can be used if different behavior is needed.

Arithmetic operators panic on overflow. `add_checked`, `add_wrapping`, `add_saturating`, and friends may be used to protect against panicking.

When choosing a bitsize and signedness of an integral type in a typical application, generally stick to `Int32` for "normal" integers; if an integer cannot be negative, eg. an image size needs to be stored, use `Uint32`. Use 64-bit sizes only when needed, such as when storing timestamps.

The `Int` and `Uint` aliases exist as reasonable defaults with a default bitsize of 32 bits, which may be configured using a compiler switch.

The `Index` alias is an integer that can be used as a valid index into a slice. The size of this type is platform-dependent; on 32-bit machines it's `Uint32`, on 64-bit machines it's `Uint64`.

Smaller bit widths should be avoided whenever possible, unless dealing with a binary format, or conserving bytes for eg. network transmission.

## Floating point types

tsuki has two floating point types: `Float32` and `Float64`. These are signed, IEEE 754-conforming binary32 and binary64 floats.

For user convenience, integer literals get converted to floating point literals whenever possible.

The operators `+`, `-`, `*`, and `/` work as defined in the standard. There's one extra operator, `**`, which is the exponentiation operator. It accepts an integer exponent on its right hand side and, for the expression `x ** n`, returns `x` multiplied by itself `n` times. When `n` is a constant, the compiler expands the expression to `n` multiplications without involving a loop.

`impl`s containing extra operations, such as square roots and trigonometry, may be found in the `std.math` module.

The `Float` alias exists as a reasonable default, with a default bitsize of 32 bits. This size can be changed using a compiler switch.

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

An atom subset may extend another subset:
```
atom Color

atom Cmyk in Color
  :cyan
  :magenta
  :yellow
  :black

atom Rgb in Color
  # shorthand form, useful for big atom subsets
  :red, :green, :blue
```
In this example, any `Cmyk` or `Rgb` value can be implicitly converted to a `Color` value, but not the other way around.

An atom subset may also be extended after it's already declared:
```
# continuing the previous example
atom Color
  :teal
  :scarlet_red
  :emerald_green
```
However, if an atom is extended in a module different from where the atom was declared, both modules have to be imported for all possible atoms to be visible.

Atoms are also used as error sets for the result type described below. An atom can be tagged as an error set by extending `Error`:

```
atom IoError in Error
  :eaccess
  :eagain
  :einval
  :enoent
  # ...
```

Error handling is described in greater detail in [Results](#results).

Sometimes, the type system isn't able to infer the correct type for an atom; in this case, the type can be specified explicitly by using the infix index operator `[]`:
```
val err = Error[:eaccess]
assert(err is Error)
```

## `Char`

`Char` is a type that can store any Unicode codepoint. Internally it's represented by a `Uint32`, however it is a distinct type from all the integer types, as characters should be treated differently from integers. Values of this type may be obtained through character literals.

## `String`

The `String` type is an immutable UTF-8 encoded string. It can be obtained through one of the available string literal syntaxes described in the Lexis section of this document.

Because all strings are UTF-8, `String` cannot be indexed directly. Instead, one of the available iterators has to be used:
```
var iter = "cześć".chars
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
var iter = "\x00\xFF".bytes
print(iter.next)  # 0
print(iter.next)  # 255
```
The `byte` method can also be used to retrieve a single byte from the string:
```
print("\x00\xFF".byte(0))  # 0
```

`String`s can be concatenated using the `~` concatenation operator:
```
print("Hello, " ~ "world!")  # Hello, world!
```

## Optionals

An optional type is a type whose value can be `nil`, or something meaningful. Values cannot be `nil` unless wrapped in this type.

An optional is written like `?T`, where `T` is the value type.

As already mentioned, optionals can be initialized to `nil`:
```
val opt: ?Int = nil
val correct: ?Int = 3
```

The presence of a value can be queried using the `?` "check" operator, and the value can later be read using the `!` unwrap operator.
```
if opt?
  print("That ain't happening, sir")

if correct?
  val ok = correct!
  print("The unwrapped value is: " ~ ok.to_string)
```

As mentioned previously in the "`if` expressions" section, there exists a shorthand for checking and unwrapping an optional, called `if val`:

```
if val ok = correct
  print("The unwrapped value is: " ~ ok.to_string)
```

## Results

Results are the primary method of handling errors in tsuki. Whenever a function can fail, it can return a result type, whose value is either an error, or a value.

A result type is written like `Err!Ok`, where `Err` is the error type, and `Ok` is the success type.

The built-in `Error` atom subset contains all possible error atoms defined throughout the program. `Error` and its subsets are the only allowed atom type in results. Objects and unions tagged with the `:: error` pragma can also be used as valid error types. Other types are not permitted as error types.

The shorthand `!T` may be used instead of `Error!T`.

For a given result type `Err!Ok`, both `Err` and `Ok` are implicitly convertible to that result type.

Examples on handling errors can be found in the "`try` expressions" and "`catch` expressions" sections of this document.

## Pointers

Pointers allow for passing _variables_ by reference. They differ from `rc` objects in that unlike an `rc` object, a pointer is not owned. To maintain memory safety, pointers can only be used in specific places, such as procedure parameters.

A mutable pointer type is written as `^var T`. An immutable pointer type is written as `^T`.

Currently, pointers can only appear inside of procedure parameter lists, to allow a procedure to modify an outside variable.

A simple example showcasing mutable pointers would incrementing an integer variable by passing its address to a procedure.
```
fun inc(x: ^var Int)
  x^ += 1

var x = 0
inc(^var x)
print(x)  # 1
```
It's also possible to create pointers to other things, such as object fields, and slice elements.

Pointers can be created using the `^var` and `^` prefix operators, and dereferenced using the `^` postfix operator. Pointers are also subject to automatic dereferencing when calling instance functions. Consider this example:
```
object Example
  val x: Int
impl
  fun print_x()
    print(.x)

fun print_x_from(example: ^Example)
  # we can dereference the pointer manually, but...
  # example^.print_x
  # ...we can also let the compiler dereference it for us
  example.print_x

var example = Example { x = 1 }
print_x_from(^example)
```

The limitations concerning pointer storage may get lifted in the future if the language gets a proper borrow checker.

## Slices

Slices allow for storing _views_ to arrays of data. In terms of semantics, they're very similar to pointers: you cannot store a slice in a permanent location, like a variable or an object; they can only appear in procedure parameters.

A slice type is written like `[T]`. A slice type with mutable elements is written like `[var T]`.

Slices are usually initialized from an owned `Seq[T]`.
```
val elems = [1, 2, 3]

fun do_stuff(slice: [Int])
  _
do_stuff(elems[..])  # slice all the elements
```
Because the slice was created from a `val Seq[Int]`, the slice itself is `[Int]`. If the source sequence was `var`, the slice would be `[var Int]`.

We can read elements from this slice by using the infix `[]` operator.
```
print(slice[0])  # 1
print(slice[1])  # 2
print(slice[2])  # 3
# print(slice[3])  # error: index out of bounds
```
Out of bounds access is checked at runtime and results in a panic.

## `Seq[T]`

`Seq[T]`, or a _sequence_, is an _owned_ dynamic array type; it can be created using the `[]` prefix operator:
```
val names = ["Jon", "John", "Josh"]
```
A sequence is _owned_, because, contrary to a slice, everyone holding a sequence _owns_ a reference to the given data. Sequences have similar memory management semantics to `rc` objects.

The usual slice operators may be used on sequences.

Mutating a sequence results in all instances of the sequence being updated:
```
var names = ["Jon", "John", "Josh"]
var names2 = names
names <- "Adam"  # push the string "Adam" to the end of the sequence
print(names2[3])  # Adam
```
To create a duplicate of the sequence, the `dup` method can be used:
```
var names = ["Jon", "John", "Josh"]
var names2 = names.dup
names <- "Adam"
print((names.len, names2.len))  # 4, 3
```

## `Table[K, V]`

`Table[K, V]`, or a _table_, is an associative array type. It maps arbitrarily-typed keys to arbitrarily-typed values. Other languages also call this type `Dictionary`, `Map`, `HashMap`.

A table can be initialized using the `{}` prefix operator. `=` is used for separating keys from values.
```
val numbers = {
  # here we create a mapping from Strings to Floats
  "pi" = 3.141592654,
  "tau" = 6.283185307,
  "e" = 2.718281828,
}
assert(numbers is Table[String, Float])
```

Tables can be indexed using the `[]` operator.
```
print(numbers["pi"])  # 3.141592654
```

Values in tables may be modified by using the `[]=` operator.
```
numbers["half"] = 0.5
```

## Tuples

Tuples are values that bind many values together. A tuple type is specified using the following syntax:
```
type Tup = (A, B, C, ...)
```
where `A, B, C, ...` is a comma-separated list of types to use for the elements of the tuple.
A single-element tuple is specified using the syntax `(A,)`. The zero element tuple `()` is treated specially, as the _unit type_, which only has a single value. The unit type is better described in [its section](#unit-type).

A tuple is instantiated using syntax similar to its type definition, with values in place of types:
```
val elems: (Int, Int, Int) = (1, 2, 3)
```
Individual elements of the tuple can be retrieved through its fields, each of which starts with an underscore `_`, followed by the zero-based index of the field:
```
print(elems._0)
```

A tuple may also be unpacked to separate variables using similar syntax:
```
val (a, b, c) = (1, 2, 3)
```
Certain elements of a tuple may be ignored while unpacking, using the `_` identifier:
```
val (x, _, z, _) = (1, 2, 3, 4)
```

A tuple's maximum allowed size is 8 fields. If more than that is needed, a named object should be used.

## Objects

An object is a named type containing user-defined fields. An object is declared like so:
```
object Example  # the name of the object
  # its fields
  var x: Int
  val y: Int       # a field can be marked as immutable
  var z, w: Float
```
The object can then be instantiated using the following construction syntax:
```
val e = Example { x = 1, y = 2, z = 3, w = 4 }
```
Normally, all fields must be initialized. However, default expressions can be provided in the object declaration:
```
object Defaults
  var a: Int = 2
  var b: Int = 4
  var c: ?Int = nil
val d = Defaults {}
```
A default value can be any expression. The expressions are evaluated every time the object is constructed. This means that expressions with visible side effects may influence the construction of different object instances:
```
var counter = 0
fun inc_counter(): Int
  val id = counter
  counter += 1
  id

object Counted
  val id: Int = inc_counter()

val a = Counted {}
val b = Counted {}
val c = Counted {}
print((a, b, c))  # (Counted { id = 0 }, Counted { id = 1 }, Counted { id = 2 })
```

There's one more exception to the "all fields must be initialized" rule. That is, certain fields may remain uninitialized until they're actually used. These fields still have to be listed in the constructor, but instead of specifying a meaningful value, `uninit` has to be used.
```
object UninitExample
  # note that the field must be var in this case, otherwise we wouldn't be
  # able to set it
  val x: Int
  var y: Int

var u = UninitExample { x = 1, y = uninit }
u.y = u.x + 2
```
It is a compile-time error to try to read from a field that's `uninit`. It is also a compile-time error to try to move an object with `uninit` fields to a different location than where it already is, eg. via `return` or setting a variable.

An object can be marked as an error type by using the `:: error` pragma.
```
object CompileError :: error
  val line, column: Index
  val message: String

val e: CompileError!Int = CompileError {
  line = 3, column = 1,
  message = "My god, what are you doing?",
}
```
Custom error types are better described in [Results](#results).

### Copying

Objects in tsuki cannot be copied by default. Instead, the `Dup` and `Copy` traits have to be implemented.

`Dup` is a trait that exposes a `dup` function for the object. This function creates a brand new *dup*licate of the object, hence the name `dup`. When `derive`d, the `dup` function will default to simply copy over the existing fields of the object to a brand new instance.

`Copy` is a special trait that the compiler interprets as a marker that this object can be implicitly copied. It also exposes a `copy` function, which, when `derive`d, defaults to simply calling `dup`. Because of this, implementing `Copy` requires `Dup` to be implemented, too.

Both traits are `derive`able, so there's no need to write the boilerplate yourself.
```
object DupExample
  var x, y: Int
impl
  derive Dup

val a = DupExample { x = 1, y = 2 }
var b = a.dup
b.y = 3
print(a)  # DupExample { x = 1, y = 2 }
print(b)  # DupExample { x = 1, y = 3 }

object CopyExample
  var x, y: Int
impl
  derive Dup, Copy

val c = CopyExample { x = 1, y = 2 }
var d = c
d.y = 3
print(c)  # CopyExample { x = 1, y = 2 }
print(d)  # CopyExample { x = 1, y = 3 }
```

### Dropping

When an object instance goes out of scope, we say it's _dropped_. An object implementation may inject some extra code to the dropping process via the `Drop` trait.

The trait is defined like so:
```
trait Drop
  fun var drop()
```

Here's an example implementation of the trait:
```
var id_count = 0
fun next_id(): Int
  val id = id_count
  id_count += 1
  id

object Dropper
  val id: Int = next_id()
impl Drop
  fun drop()
    print("Dropping " ~ .id.to_string)

val d = Dropper {}
val e = Dropper {}
var f = Dropper {}
f = Dropper {}  # Dropping 2
# Dropping 3
# Dropping 1
# Dropping 0
```
The order of dropping objects in variables is from last to first declared variable in a given scope.

### Inheritance

Objects in tsuki have single inheritance, which means that any given object may inherit from another object, but not multiple objects.
An object that inherits from a parent inherits all of the parent's fields, functions, and type functions. A parent object's `impl` influences functions available in all of its children.

An object can inherit from another object using the `of` keyword. Objects that do not inherit from anything _also_ cannot be inherited from; each object that should be inheritable should inherit from the `Root` object:
```
object Parent of Root
  val x: Int

object Child of Parent
  val y: Int

val c = Child { x = 1, y = 2 }
```

Object initialization can be _chained_ by using the `of` operator inside of a constructor:
```
fun init_parent(): Parent
  Parent { x = 1 }

val c = Child { of init_parent(), y = 2 }
```
`of` must appear at the very beginning of a constructor, before all the fields.

### Reference counting

tsuki allows for automatic memory management via _reference counting_. An object can be marked as reference counted by using the `rc` keyword after the name and parent object.
```
object Example rc
  val x: Int32

val ref = Example { x = 1 }
```

A reference counted object is always allocated on the heap, and stores a `refcount: Uint64` along with the object data. This field starts out at 1, and is increased with each new reference to the object. Similarly, it's decreased whenever a reference to the object is dropped. Once the reference count reaches 0, the object is freed from memory, and its `Drop` trait is called if applicable.

Sometimes increasing the reference count is undesirable, eg. when implementing a doubly linked list. Such a list contains cycles, which reference counting cannot deal with. In this case, the `weak` type annotation may be used.

```
object DoubleLink[T]
  var prev: ?DoubleLink[T] weak
  var next: ?DoubleLink[T]
  val value: T

var a = DoubleLink[Int] {
  prev = nil,
  next = uninit,
  value = 1
}
var b = DoubleLink[Int] {
  prev = a,
  next = nil,
  value = 2,
}
a.next = b
```

## Unions

A union is a type, tagged with a _kind atom_, containing a single, dynamically-chosen value from a fixed set of possible variants. tsuki unions are akin to C unions, but with an extra type tag present.

A union is declared using the `union` keyword, followed by the name of the union, continued with a block containing all the possible variant tuples of the union with their types.

```
union Shape
  :rectangle(Float, Float, Float, Float)  # x, y, width, height
  :circle(Float, Float, Float)  # x, y, radius
```

A union can then be initialized using the union initialization syntax: the union type, followed by the an atom literal specifying the variant, followed by the variant values in parentheses.

```
var my_shape = Shape:rectangle(32, 32, 64, 64)
```

The [`match` expression](#match-expression) can be used to execute code blocks depending on union variants. See its section for more information.

## Type aliases

It's possible to alias types using the `type` declaration. This can be useful when dealing with very long nested generic types.

```
type Strings = Table[String, String]
```

Type aliases defined inside of implementations are called _associated types_, and are described in [Implementations](#implementations-impl).

## Implementations (`impl`)

`impl` blocks allow for specifying types and functions associated with types, or instances of types. They also allow for implementing traits.

An `impl` block has two forms: the shorthand form, which can be used directly after an object declaration or another shorthand implementation, and the full form, which must be used everywhere else.

The following variants of the full form are available:
```
impl MyObject
impl type MyObject
impl MyTrait for MyObject
```
The first variant defines functions on instances of `MyObject`. The second variant defines functions on the `MyObject` type itself. The third variant implements the trait `MyTrait` on instances of `MyObject`.

Functions defined on _instances_ of types are referred to as _instance functions_.

Types defined both on types and instances of types are referred to as _associated types_. Note that it does not matter whether a type is declared in an `impl`, or an `impl type`: an associated type is _always_ bound to the `impl type`. Associated types can be referred to using `Self.T`, where `T` is the type name.

The shorthand form variants are similar, but omit the destination type name:
```
object MyObject
impl
impl type
impl MyTrait
```
Note how these implementations have to be defined right below an object declaration. This is because the shorthand form is actually part of the object declaration syntax.

Shorthand declarations end with the first token that is not `impl`, or with full `impl` that contains a type name.

`impl type` may be used to define functions inside of the object type itself, essentially using the type as a namespace.

```
object MathStuff
impl type
  fun add_two(x: Int): Int
    x + 2

print(MathStuff.add_two(2))  # 4
```

`impl` blocks are bound to modules, rather than types - if an `impl` block appears in a module different to where the type was declared, that module has to be imported alongside the module declaring the type for all functions to be available.

### `Self`

The `Self` type is a special type only accessible in `impl` blocks. This type refers to the type that's currently being `impl`emented.

Note the uppercase letter, though; this type is _not_ to be confused with the `self` variable in instance functions.

As already noted, `Self` is the only way of referring to associated types.

### `self`, `fun var`, and `fun val`

Non-`type` blocks implement functions on _instances_ of a given type. This instance can be accessed, and optionally mutated inside of the function, by using the special, implicit `self` variable.

Fields inside of the object can be accessed using the special `.field_name` notation. Note that fields can _only_ be accessed inside of the object, and can only be made public by using getters and setters (described later).

```
object Example
  val x: Int
  var y: Int
impl
  # getter for x
  fun x(): Int
    .x
  # getter for y
  fun y(): Int
    .y
  # setter for y
  fun y=(val: Int)
    .y = val
```

This notation can also be used to invoke functions on `self`:

```
impl Example
  fun inc_x()
    .x += 2

  fun inc_x_twice()
    .inc_x()
    .inc_x()
    # syntax sugar for
    # self.inc_x()
    # self.inc_x()
```

`self` cannot be used to access fields; it can only be used for invoking methods.

Now, about `fun val` and `fun var`: these are annotations that let the compiler know whether `self` must be `var` or not for the function to be callable. This property is usually inferred automatically, depending on what a given function does.

- If a function modifies any field directly, then it's `var`.
- If a function calls another instance function that's `var`, then it's `var`.
- If a function calls another non-instance function that accepts `^var Self` as a parameter, then it's `var`.
- Otherwise, the function is `val` because it cannot possibly modify `self`.

`fun val` and `fun var` can still be specified explicitly for readability purposes. In traits, specifying the `var`ness of `self` is necessary, because the compiler cannot infer `var`ness without looking at a function's body.

### Getters and setters

tsuki has syntactic support for getters and setters.

If an instance function doesn't accept any parameters beyond `self`, the parentheses may be omitted when calling it. This is the preferred style of calling zero-argument instance functions.

```
var e = Example { x = 1, y = 2 }
e.inc_x_twice
```
Note that this doesn't work with the shorthand `.func()` notation inside of implementations, to avoid confusing fields with instance functions.

Instance functions may also be named with a trailing `=`. In this case, the function is a _setter_, and may only have a single parameter - the value that a given property should be set to.

```
object SetterExample
  var x: Int
impl
  fun x=(val: Int)
    print("Setting x to " ~ val.to_string)
    .x = val

var e = SetterExample { x = 1 }
e.x = 2
```

Because writing getters and setters can quickly get old, tsuki provides some syntax sugar for generating them automatically.

```
object AutoGetSet
  var x, y: Int
  val z: Int
impl
  getset x, x=, y, y=, z
```
The `getset` declaration will automatically generate getters and setters for the provided fields. A name without a `=` generates a getter, and a name with a `=` generates a setter. Note that setters cannot be created for `val` fields; this is (obviously) because `val` fields are read-only.

### `derive`

The compiler allows for some trait implementations to be _derived_ automatically, only using a type implementation. This can be done using the `derive` declaration inside of an `impl` block.

```
object Copycat
  val x: Int
impl
  derive Dup, Copy
```



## Traits

Traits allow for creating constraints on what functions and associated types a type must implement. A trait is declared using the `trait` keyword.

```
trait Animal
  type Food
  fun val speak()
```
The above code defines a trait `Animal` that requires the implementing type to have a `Food` type and a `speak()` function, where `self` is a `val`.

Traits can be used as generic constraints:
```
fun make_it_speak[T: Animal](animal: ^T)
  animal.speak
```

Traits can also be used like interfaces, where the actual type is determined at runtime, provided they're behind a pointer.
```
fun make_it_speak_at_runtime(animal: ^Animal)
  animal.speak
```

Traits can inherit from other traits by using the `of` keyword, similarly to objects.

```
trait DomesticAnimal of Animal
  fun var feed(food: Self.Food)
```

It's also possible to create a type constraint that requires multiple traits to be implemented, using the `and` operator, like `T and U and V`.

Associated types can also be constrained by using the familiar `:` notation.

```
trait AnimalFood
  fun var feed(animal: ^Animal)

trait Animal
  type Food: AnimalFood
  fun val speak()
```

### Operator overloading

Several built-in traits exist that allow for overloading existing operators. Here's a list of all the traits, their function names, and the operators they overload:

| Operator | Trait | Function | Meaning |
| -------- | ----- | -------- | ------- |
| unary `not` | `Not` | `unot` | boolean NOT |
| unary `~` | `BitNot` | `bnot` | bitwise NOT |
| unary `-` | `Neg` | `neg` | arithmetic negation |
| unary `?` | `Check` | `check` | optional/result is present/successful |
| unary `!` | `Unwrap` | `unwrap` | unwrap optional/result |
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
| binary `..` | `UpTo` | `up_to` | exclusive range `[x; y)` |
| binary `..=` | `UpToIncl` | `up_to_incl` | inclusive range `[x; y]` |
| binary `==` | `Equal` | `equal` | value equality |
| binary `<` | `Less` | `less` | number comparison |
| binary `<=` | `LessEqual` | `less_equal` | |
| binary `in` | `Contains` | `contains` | presence in a list/set |
| binary `[]` | `Index` | `at` | indexing (slicing) |
| binary `{}` | `IndexDup` | `at_dup` | indexing (duplicating) |
| ternary `[]=` | `Put` | `put` | setting values at an index/range |
| ternary `{}=` | `PutAlt` | `put_alt` | `Put`, alternative version |
| binary `<-` | `Push` | `push` | insertion into a list/set |

Other operators are not user-overloadable (such as `and`, `or`), or derived from other operators (such as `>` and `>=`, derived from `<` and `<=` by flipping the arguments around).

Unary operator traits have a single associated type `Ret`, which signifies the result type of the operator. These traits are defined like so (example based on unary `not`):
```
trait Not
  type Ret
  fun val unot(): Self.Ret
```
The only exception here is `Check`, whose `Ret` argument is always `Bool`.

Binary operator traits have a single generic argument `R`, and an associated type `Ret`. `R` specifies the right-hand side of the operation, and `Ret` is the result of the operation. The traits are defined like so (example based on binary `Add`):
```
trait Add[R]
  type Ret
  fun val add(rhs: R): Self.Ret
```
The only exception to this definition is `Push`, which accepts a `var self` and is expected to mutate the destination object.

Ternary index operator traits have two generic arguments: `I`, and `V`, where `I` is the index type, and `V` is the value type. These operators do not return anything, so there is no associated `Ret` type.
The functions for these operators require a `var self`.
```
trait SetIndex[I, V]
  fun var set_at(index: I, value: V)
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

To allow for calling functions on these symbols, the generic type has to be constrained using traits. Generic constraints look like regular type annotations:
```
fun add_two[T: Add[Int]](x: T): T.Ret
  x + 2
```

Generic parameters are subject to type propagation, similar to function parameters. However, in generics, this might pose a problem, as a generic type may not have any constraints put onto it; in this case, the semicolon `;` separator can be used:
```
object ReversedTable[V; K: Hash]
```

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
import std.io for stdin, stdout
```
It's also possible to not import _any_ symbols into the scope, and instead force full qualification:
```
import std.io for _
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
import tools/utility
```

An external package manager may instruct the compiler to treat a directory as an external package. By default, the directory containing the source code of the standard library, `std`, is such a package.

Modules from external packages can be imported using a dot `.`:
```
import std.os
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

# objects have to be marked public
pub object Adder
  # object fields _cannot_ be public...
  val increment: Int

impl
  # ...but getters and setters can
  pub getset increment

  # functions inside of `impl` also must be marked public
  pub fun add(to: Int): Int
    adding_abstraction(to, .increment)

impl type
  pub fun init(increment: Int): Self
    Self { increment = increment }
```

Package: my_program, File: src/my_program.tsu
```
import adder

val two = Adder.init(2)
print(two.add(2))  # 4
```

It's also possible to reexport some imported symbols, by using `pub` before an `import` statement.
```
pub import std.math
```

# Documentation

Documentation in tsuki is similar to that in Rust, with a flavor of Javadoc. Doc comments may appear before symbol declarations, and use GitHub Flavored Markdown for text. Let's take the previous example with the `Adder` object and document it:
```
fun adding_abstraction(a, b: Int): Int
  a + b

## An adder adds a constant increment to a provided value.
pub object Adder
  val increment: Int

impl
  ## Retrieves the increment of the adder.
  pub getset increment

  ## Adds the increment the adder was initialized with to `to`.
  ##
  ## @param to  The value to increment.
  ## @return    The incremented value.
  ##
  ## @see Adder.init
  pub fun add(to: Int): Int
    adding_abstraction(to, .increment)

impl type
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
The first paragraph in a doc comment is the _brief_ of a given symbol. The purpose of the brief is to give a brief overview of what the symbol's purpose is.

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

# Macros

TODO: still to be discussed with others, need some opinions on how a _good_ macro system should work.

# C FFI

TODO: not important right now, and I'm leaving home for the weekend so I don't really have time to finish this.