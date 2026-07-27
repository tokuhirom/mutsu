# A CStruct field named like a builtin method is unreachable

A field of an `is repr('CStruct')` class whose name collides with a builtin
method cannot be read or written through the accessor — the builtin answers
first:

```raku
use NativeCall;
class Body is repr('CStruct') { has int64 $.first is rw; }
sub calloc(size_t, size_t --> Pointer) is native { * }
my $b = nativecast(Body, calloc(1, 16));
$b.first = 7;
say $b.first;      # raku: 7    mutsu: Body.new
```

`Body.new` is the invocant coming back out of the *list* `.first`, which treats
a non-list as a one-element list.

## Why

A CStruct handle keeps no Raku attributes: its fields live in the C struct its
`address` points at, so `cstruct_field_value` / `cstruct_field_assign` are
reached from the **accessor fallback** in `runtime/methods_instance_ops.rs`,
which runs *after* builtin method dispatch. A plain Raku class is unaffected —
its declared attribute is found on the earlier declared-method path
(`class C { has $.first }` works in both implementations).

## Why it is not a one-liner

The fix is an ordering change on a hot path: the field lookup has to happen
before builtin method dispatch for instances of a registered CStruct class. That
means either a cheap "is this class a CStruct?" gate early in the dispatch chain,
or hoisting the whole native-handle branch. Both touch method dispatch for every
instance method call, so it wants a measurement, not a quick patch.

## How much it matters

Not currently blocking. The field names in the bindings mutsu runs (`DBIish`'s
`MYSQL_BIND`: `length`, `is_null`, `buffer`, `buffer_type`, `error`, …;
OpenSSL's structs) do not collide with builtins — `length` is not a mutsu builtin
method. Found while writing `t/nativecall-typed-pointer.t`, whose first draft
used `$.first`/`$.second` and had to rename them.
