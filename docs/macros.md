# El Macro

> A Spanish guy, knowing all about how good macro systems work.

As the compiler implementation progresses, I'll be noting here some things about the macro system.

## Macro declaration

The declaration of a macro is similar to a function, except it only ever takes a single argument:

```
import @std.macros as _

macro my_macro(ctx: macros.Context): macros.AnyResolvedNode
:: macros.position(:call)

```

The task of the macro is to transform the nodes found in the environment into a final node. There are a few things to note here:
- The `env` argument tells the macro about the context it's being called in, including the _full_ call site, and type system things like the expected return type.
- `macros.position` is a pragma that specifies _how_ the macro can be called. Depending on what this is, different context is going to be available in the environment.
   - For instance, if `:call` is passed, the AST of the Call node is going to be available `my_macro(arg1, arg2)`.
   - On the other hand, if `:pragma(:fun)` is passed, the macro will only apply to pragmas attached to functions, and is going to receive the whole function's AST.

The kind of the AST node output by the macro has to be valid for the call site, which is ensured by the API itself.

## The context

The `macros.Context` object is responsible for holding information related to the callsite of the macro. It has a few functions:

- `fun callsite(self): macros.CallNode` returns a handle to a node that represents the macro's callsite.
- `fun callsite_env(self): macros.Environment` returns a handle to the semantic checking environment positioned at the call site.
- `fun this_env(self): macros.Environment` returns a handle to the semantic checking environment positioned at the macro's definition.
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
While we're on the topic of functions, let's talk _calls_. Note that I have omitted nullary, unary, and binary operators. This is because they all desugar to calls, and by the time we arrive to macro evaluation, they are no longer part of the AST. Instead we substitute them with calls:
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
object Unresolved
object ResolvedToExpression
object ResolvedToStatement

# I am aware that nobody likes Java-long names, but readability is important.
# Users do not touch these traits anyways.
object DoNode[S]
where S: ControlFlowResolutionState
   # details omitted

trait ControlFlowResolutionState

impl ControlFlowResolutionState for ResolvedToExpression
impl ControlFlowResolutionState for ResolvedToStatement

impl UnresolvedNode for DoNode[Unresolved]
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
impl Environment
   fun resolve[T](self, node: T): T.Resolved
   where
      T: UnresolvedNode

      _  # details omitted

   fun lookup(self, identifier: String): AnySymbolNode
      _  # details omitted
```

The behavior of `lookup` depends on the environment being used; when used on `callsite_env`, identifiers are going to be looked for at the callsite. When used on `this_env`, identifiers are going to be looked for at the macro's declaration site.

- note for self: I don't like the lack of hygiene with using callsite_env, maybe we need to introduce another kind of identifier node for differentiating between identifiers passed as input vs identifiers created by the macro?

# Questions for later

- We need some sort of `gen_sym` for hygiene.
- Describe how symbol nodes work. Since they are resolved identifiers, they allow for accessing things like type information, but how?
