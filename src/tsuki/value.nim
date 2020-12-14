type
  ObjectImpl[V] = ref object
    # this is defined as a generic type to avoid repetition.
    vtable*: int
    fields: seq[V]

  NimData* = object
    ## A Nim object, as stored in the tsuki VM.
    vtable*: int
    data*: ref RootObj

proc `[]`*[V](obj: ObjectImpl[V], field: int): V =
  ## Returns an object field. Manual usage of this should be avoided at all
  ## costs! This is defined for internal use in the VM.
  obj.fields[field]

proc newObjectImpl[V](vtable, size: int): ObjectImpl[V] =

  result = ObjectImpl[V](vtable: vtable)
  result.fields.setLen(size)

const
  vtableNil* = 0
    ## The vtable ID for ``Nil``.
  vtableBool* = 1
    ## The vtable ID for ``Bool``.
  vtableFloat* = 2
    ## The vtable ID for ``Float``.
  vtableString* = 3
    ## The vtable ID for ``String``.
  vtableFirstObject* = 8
    ## The first vtable ID of user-defined objects.

when defined(nimdoc):

  type
    ValueImpl = object

    Object* = ObjectImpl[Value]
      ## A tsuki object.

    Value* = ValueImpl
      ## A tsuki value. What the byte representation is depends on compile-time
      ## options. ``-d:tsukiNanBoxing`` enables the usage of NaN-boxing for
      ## values: in this case, this is defined as ``distinct float64``.
      ## Otherwise, it's a variant object with a ``kind`` field and a value.

  proc isNil*(v: Value): bool =
    ## Returns whether the value is ``nil``.

  proc isFalse*(v: Value): bool =
    ## Returns whether the value is strictly ``false``.

  proc isTrue*(v: Value): bool =
    ## Returns whether the value is strictly ``true``.

  proc isFloat*(v: Value): bool =
    ## Returns whether the value is a ``Float``.

  proc isString*(v: Value): bool =
    ## Returns whether the value is a ``String``.

  proc isObject*(v: Value): bool =
    ## Returns whether the value is an object.

  proc isNimData*(v: Value): bool =
    ## Returns whether the value stores Nim data.

  proc isFalsey*(v: Value): bool =
    ## Returns whether the value is _falsey_. The only falsey values are
    ## ``nil`` and ``false``.

  proc isTruthy*(v: Value): bool =
    ## Returns whether the value is _truthy_. All values except ``nil`` and
    ## ``false`` are truthy.

  proc getBool*(v: Value): bool =
    ## Returns the ``bool`` representation of the value. The value *must* be
    ## either ``true`` or ``false``, otherwise an assertion is triggered.

  proc getFloat*(v: Value): float64 =
    ## Returns the ``float64`` representation of the value. The value *must* be
    ## a ``Float``, otherwise an exception is raised.

  proc getString*(v: Value): string =
    ## Returns the ``string`` representation of the value. The value *must* be
    ## a ``String``, otherwise an exception is raised.

  proc getObject*(v: Value): Object =
    ## Returns the ``Object`` representation of the value. The value *must* be
    ## a tsuki object, otherwise an exception is raised.

  proc getNimData*(v: Value, T: type): T =
    ## Returns the user data stored in the value. The value *must* contain
    ## user data, otherwise an exception is raised.
    ##
    ## All user data in tsuki values is internally stored behind ``ref``s.
    ## This serves two purposes:
    ## - leveraging Nim's automatic memory management
    ## - making ``Value`` have a constant size
    ##
    ## Note that tsuki does not play very well with ARC. Although cyclic
    ## refs are either avoided or annotated with the ``{.cursor.}`` pragma,
    ## user objects *may* still form cycles, so it's better to use a memory
    ## management model that supports them (such as the default GC).

  proc mgetNimData*(v: Value, T: not ref): var T =
    ## Returns a mutable reference to the user data stored in the value.
    ## The value *must* contain user data, otherwise an exception is raised.

  const
    tsukiNil* = Value()
      ## Constant representing a nil value.
    tsukiTrue* = Value()
      ## Constant representing ``true``.
    tsukiFalse* = Value()
      ## Constant representing ``false``.
      ##
      ## These constants are ``let``s when using the variant object
      ## representation.

  converter toValue*(b: bool): Value =
    ## Converts a ``bool`` to a tsuki ``Value``

  converter toValue*(f: float64): Value =
    ## Converts a ``float64`` to a tsuki ``Value``.

  converter toValue*(s: string): Value =
    ## Converts a ``string`` to a tsuki ``Value``.

  proc toValue*[T](data: T): Value =
    ## Converts user data to a tsuki ``Value``. Unlike the other converters
    ## this is explicit because you usually do not want to haphazardly convert
    ## your objects into tsuki values.

  proc newObject*(vtable, size: int): Value =
    ## **Warning: Internal use only.** Creates an object value.

  proc vtable*(v: Value): int =
    ## Returns the vtable ID of the value's type. This can be used to uniquely
    ## identify a value's type **in a single assembly**.

  proc `$`*(v: Value): string =
    ## Stringifies a value.

  proc `==`*(a, b: Value): bool =
    ## Compares two values for equality.

elif defined(tsukiNanBoxing):

  {.error: "nan boxing is not implemented yet".}

else:

  type
    ValueKind = enum
      vkNil
      vkFalse
      vkTrue
      vkFloat
      vkString
      vkObject
      vkNimData

    Object* = ObjectImpl[Value]

    Value* = object
      case kind: ValueKind
      of vkNil, vkFalse, vkTrue: discard
      of vkFloat: floatVal: float64
      of vkString: stringVal: string
      of vkObject: objectVal: Object
      of vkNimData: nimData: NimData

  {.push inline.}

  proc isNil*(v: Value): bool = v.kind == vkNil

  proc isFalse*(v: Value): bool = v.kind == vkFalse

  proc isTrue*(v: Value): bool = v.kind == vkTrue

  proc isFloat*(v: Value): bool = v.kind == vkFloat

  proc isString*(v: Value): bool = v.kind == vkString

  proc isObject*(v: Value): bool = v.kind == vkObject

  proc isNimData*(v: Value): bool = v.kind == vkNimData

  proc isFalsey*(v: Value): bool = v.kind <= vkFalse

  proc isTruthy*(v: Value): bool = v.kind > vkFalse

  proc getBool*(v: Value): bool =

    assert v.kind in {vkFalse, vkTrue}
    v.kind == vkTrue

  proc getFloat*(v: Value): float64 = v.floatVal

  proc getString*(v: Value): string = v.stringVal

  proc getObject*(v: Value): Object = v.objectVal

  proc getNimData*(v: Value, T: type): T =

    when T is ref:
      cast[T](cast[pointer](v.nimData.data))
    else:
      cast[ref T](cast[pointer](v.nimData.data))[]

  proc mgetNimData*[TT: not ref](v: Value, T: type TT): var T =
    cast[ref T](cast[pointer](v.nimData.data))[]

  let
    tsukiNil* = Value(kind: vkNil)
    tsukiTrue* = Value(kind: vkTrue)
    tsukiFalse* = Value(kind: vkFalse)

  converter toValue*(b: bool): Value =
    Value(kind: vkFalse.succ(ord(b)))

  converter toValue*(f: float64): Value =
    Value(kind: vkFloat, floatVal: f)

  converter toValue*(s: string): Value =
    Value(kind: vkString, stringVal: s)

  proc initValue*[T](vtable: int, data: T): Value =
    assert vtable >= vtableFirstObject
    result = Value(kind: vkNimData, nimData: NimData(vtable: vtable))
    when T is ref:
      result.nimData.data = cast[ref RootObj](cast[pointer](data))
    else:
      var r = new(T)
      r[] = data
      result.nimData.data = cast[ref RootObj](cast[pointer](r))
    GC_ref(result.nimData.data)

  proc newObject*(vtable, size: int): Value =
    Value(kind: vkObject, objectVal: newObjectImpl[Value](vtable, size))

  const vtableForKind = [
    vkNil: vtableNil,
    vkFalse: vtableBool,
    vkTrue: vtableBool,
    vkFloat: vtableFloat,
    vkString: vtableString,
  ]

  proc vtable*(v: Value): int =

    case v.kind
    of vkNil..vkString:
      vtableForKind[v.kind]
    of vkObject:
      v.objectVal.vtable
    of vkNimData:
      v.nimData.vtable

  {.pop.}

  proc `$`*(v: Value): string =
    case v.kind
    of vkNil: "nil"
    of vkFalse: "false"
    of vkTrue: "true"
    of vkFloat: $v.floatVal
    of vkString: v.stringVal
    of vkObject: "<object>"
    of vkNimData: "<nimdata>"

  proc `==`*(a, b: Value): bool =
    if a.kind == b.kind:
      result =
        case a.kind
        of vkNil, vkFalse, vkTrue: true
        of vkFloat: a.floatVal == b.floatVal
        of vkString: a.stringVal == b.stringVal
        of vkObject: a.objectVal == b.objectVal
        of vkNimData: a.nimData == b.nimData
    else:
      result = false
