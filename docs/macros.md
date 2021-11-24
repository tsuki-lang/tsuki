# El Macro

> A Spanish guy, knowing all about how good macro systems work.

As the compiler implementation progresses, I'll be noting here some things about the macro system.

## Macro declaration

The declaration of a macro is similar to a function, except it only ever takes a single argument:

```
import @std.macros as _

macro my_macro(ctx: macros.Context[:call]): macros.Error!macros.AnyResolvedNode
   _  # do things
```

The task of the macro is to transform the nodes found in the environment into a final node. There are a few things to note here:
- The `env` argument tells the macro about the context it's being called in, including the _full_ call site, and type system things like the expected return type.
- The type of this argument is `macros.Context[P]`. The `P` parameter is a `const macros.CallPosition`, which specifies _where_ the macro can be called.
   - For instance, `:call` signifies that the macro is called like a regular function, `my_macro(arg1, arg2)`.
   - Other positions include `:fun_pragma`, `:object_pragma`, `:union_pragma`, `:atom_pragma`, `:type_pragma`, and `:derive`.
      - While the `:*_pragma` kinds are self-explanatory, `:derive` is special, because it can be used together with the `derive` declaration inside `impl` blocks. It's used for deriving trait implementations automatically.

The kind of the AST node output by the macro has to be valid for the call site, which is ensured by the API's strong typing, described later.

## The context

The `macros.Context` object is responsible for holding information related to the callsite of the macro. It has a few functions:

- `fun callsite(self): macros.CallNode` returns a handle to a node that represents the macro's callsite.
- `fun callsite_env(self): macros.Environment[:call]` returns a handle to the semantic checking environment positioned at the call site.
- `fun this_env(self): macros.Environment[:definition]` returns a handle to the semantic checking environment positioned at the macro's definition.
- `fun expected_return_type(self): ?macros.Type` returns the type that is expected at the position of the callsite. This is non-nil only in expression macros.

## Nodes

All operations on nodes within macros must always produce correct AST. This is ensured through type-safe handles to nodes, [inkwell](https://github.com/TheDan64/inkwell)-style. For instance, you can modify the first node of a `CallNode` (the called function), and push more nodes into it, but you cannot modify its second node.

Additionally, the nodes you push into a `CallNode` must all satisty `Expression`, which is a trait implemented by all nodes that are valid in expression position. To allow for runtime switching on expressions, the

Unary and binary operators should probably be represented as unions internally (the variants shouldn't be publicly visible), and their type should be discriminated by an atom. For instance, to construct an addition, you can do `BinaryNode.new(:add, left, right)`. To construct a pointer dereference, you can do `UnaryNode.new(:deref, left)`. Note that these operators all desugar to the small subset of AST available to macros, as described below.

### The canonical representation

Macros should only ever deal with a subset of the full AST, one that's mostly portable through compiler versions. We call that subset the _canonical representation_.
Additionally, AST that is input into the macro is not semantically checked beforehand, and AST that is output by the macro must always be semantically checked using the macro's environment.

I don't know what's the exact set of nodes that will be exposed to macros, but I can imagine all literals being included, like:
```
# before sem
True
False
Integer
Float
Identifier

# both before and after sem
String
Atom

# after sem
Bool
Uint8
Uint16
Uint32
Uint64
Int8
Int16
Int32
Int64
Float32
Float64
Symbol
```
In addition to that, we need control flow:
```
# untyped
Do
If

# shared
IfBranch
ElseBranch

# typed

# NB: the separation here is needed so that we can ensure that the last statement in an expression
# block is an expression statement
DoExpression
DoStatement

IfExpression
IfStatement

While
For
```
Also, definitions:
```
Fun
Object
Union
AtomSet
Type
```
While we're on the topic of functions, let's talk _calls_. For API simplicity, untyped operators desugar to three different node kinds: `Nullary`, `Unary`, and `Binary`.
```
# these are macro-exclusive, untyped, generalized versions of operators.
# their extra is an atom specifying what operator the node refers to
Nullary
Unary
Binary

# untyped
Call

# typed
ResolvedCall
```

### Type-level separation between unresolved and resolved nodes

Certain types of nodes can be unresolved or resolved, and in these cases we want to separate between them at the type level. For that, we'll use atoms in generic parameters.

```
# I am aware that nobody likes Java-long names, but readability is important.
# Users do not touch this atom anyways.
atom ControlFlowResolutionState
   :unresolved
   :resolved_to_expression
   :resolved_to_statement

object DoNode[S]
where S: ControlFlowResolutionState
   # details omitted

impl UnresolvedNode for DoNode[:unresolved]
   # details omitted

impl[S] ResolvedNode for DoNode[S]
where S: ControlFlowResolutionState
   # details omitted
```

This way we can avoid creating a lot of types for each state a node kind can be in; we use generic `impl`s to only implement specific sets of functionality for nodes that satisfy specific states.

### Resolving nodes

An `UnresolvedNode` can become a `ResolvedNode` if it is passed through an `Environment` for semantic analysis. An important distinction to make is that `ResolvedNode` trees are **incompatible** with `UnresolvedNode` trees! A resolved tree must be resolved _fully_, it cannot contain any unresolved nodes inside. This is safe-guarded by the type system.

An environment exposes a few functions for resolving (semantically analyzing) nodes:
```
object Environment[K]
where K: EnvironmentKind
   # details omitted

atom EnvironmentKind
   :call        # callsite
   :definition  # definition site

impl[K] Environment[K]
   fun resolve[T](self, node: T): T.Resolved
   where
      T: UnresolvedNode

      _  # details omitted

impl Environment[:call]
   fun lookup(self, identifier: IdentifierNode): AnySymbolNode
      _  # details omitted

impl Environment[:definition]
   fun get(self, identifier: String): AnySymbolNode
      _  # details omitted
```

Note that there are two separate kinds of environments that can be used for looking things up. As already mentioned in the section describing [the context](#the-context), an environment points either to the callsite, or the definition site. Any environment can be used for semantic checking, and its scope will be used for looking up identifiers.

#### Symbol creation

Macros cannot create bare identifiers. They can only process existing ones input into them, but all identifiers created by macros must already be resolved. In fact, all symbols created by macros come as part of full declarations. For instance, it's impossible to create a variable symbol that does not have a corresponding declaration, because then the scope of the variable is not clear.

One thing that's _not_ compile-time checked with variables is their scope. The API will allow you to generate the following code:
```
print(<symbol 1>)
val <symbol 1> = 10
```
Note how `<symbol 1>` is undeclared in the `print`; the `val` was created, then the `print` was added into the AST, and then the `val` was added afterwards.

Unfortunately there is no way to model this in tsuki's type system, at least not that I know of. The compiler will reject this code before any code generation is performed, but the error message may be unclear, as the AST resulting from macros may not have proper span information.

## Symbols

Symbol nodes represent identifiers that have meaning. This meaning may be type information, which is possessed by _all_ symbols, as well as extra metadata on what the symbol is more concretely.

This information may be queried by using getters defined on `macros.AnySymbolNode`:
```
union AnySymbol
   :variable(VariableSymbol)
   :fun(FunSymbol)
   :object(ObjectSymbol)
   :union(UnionSymbol)
   :atom(AtomSymbol)

object AnySymbolNode
   # details omitted

impl AnySymbolNode
   ## Returns the symbol stored in the node.
   fun symbol(self): AnySymbol
```

The `AnySymbol` union encapsulates all the possible symbol kinds in a set that's easy to `match` over. Additionally, convenience methods are provided for converting to the inner values, for use with `if val` and the like:

```
impl AnySymbol
   fun as_variable(self): ?VariableSymbol
      _  # details omitted

   fun as_fun(self): ?FunSymbol
      _  # details omitted

   fun as_object(self): ?ObjectSymbol
      _  # details omitted

   fun as_union(self): ?UnionSymbol
      _  # details omitted

   fun as_atom(self): ?AtomSymbol
      _  # details omitted
```

## Error handling

Using `panic` in macros is forbidden, because it leads to a bad user experience. All options, results, and the like, must be unwrapped explicitly.

Upon encountering invalid input, a macro can return an `:error` result with a `macros.Error` inside. This type stores information about the span the error covers, as well as the error message.

Because tsuki's control flow analysis is quite simple (at least in the early stages), it cannot infer what values are possible for a given variable in a given branch of the program. This is why `unreachable()` exists; it's to mark these spots as unreachable, and if a given spot is reached, the program panics. But panicking is strictly forbidden inside macros, so the `@std.macros` module provides replacements for these common tasks.

`AnyNode` (which is implemented by _all_ node kinds) provides a function `error`, whose sole purpose is to produce errors:
```
impl[T] T
where
   T: AnyNode

   fun error(self, message: String): macros.Error
      _  # details omitted
```

Additionally, a few standalone macros exist that create an error, whose span is the callsite:
```
# Accepts a single String argument with an error message, and produces an error whose span is
# the callsite.
macro macro_error(ctx: Context[:call]): Error!ResolvedNode
   _  # details omitted

# Accepts no arguments, and produces an error with the message "unreachable code reached".
macro unreachable(ctx: Context[:call]): Error!ResolvedNode
   _  # details omitted
```

This means that usual control flow:
```
if some_cool_condition
   # some_cool_condition guarantees that my_based_value is not :cringe, but the compiler can't
   # figure that out
   match my_based_value
      :based -> _  # do stuff
      :cool -> _   # do more stuff
      :cringe -> panic("there is no cringe in this program")
```
turns to this:
```
import @std.macros for macro_error

if some_cool_condition
   match my_based_value
      :based -> _
      :cool -> _
      :cringe -> return macro_error("there is no cringe in this program")
```
