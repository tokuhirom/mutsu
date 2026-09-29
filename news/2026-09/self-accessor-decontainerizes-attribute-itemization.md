# `self.x` decontainerizes; `$!x`/`$.x` keep their own itemization

Two related bugs in how a `$`-sigil attribute's container travels through a
read (#9807).

## `self.x` was not decontainerizing

Raku's generated accessor for `has $.x` decontainerizes its return value:
`self.x` is not itemized, only `$.x` (defined as `self.x.item`) is
(`Language/objects.rakudoc`: "there is a difference between `self.a` and
`$.a`, since the latter will itemize"). So `has $.x = (1, 2, 3)` iterates
three times under `.say for self.x` but once, as a single `(1 2 3)`, under
`.say for $.x`.

mutsu's fast accessor-read path (`try_fast_accessor_read`) and its
interpreter fallback returned the stored value's own item-ness unchanged, so
both forms printed `(1 2 3)` once. The fix decontainerizes an
`ItemArray`/`ItemList` value on read, through a new
`Interpreter::accessor_read_value` helper — mirroring the existing
`itemize_attr_store_value` on the write side — unless the accessor is
declared `is rw`, whose default accessor hands back the writable Scalar
itself (`Type/Attribute.rakudoc`: "the default accessor... will return a
writable value"), item-ness included.

## A constructor-provided Array/Hash was not itemized into its `$` attribute

A `$` attribute is a Scalar container, so an Array/Hash value ends up
holding the itemizing `$` prefix once stored — already true for a `has
$.x = <default>` initializer (#9040), but not for a value supplied through
`.new(x => ...)`/`.bless(x => ...)`: three separate construction paths
(the native no-BUILD fast path, `dispatch_new`'s interpreter path, and
`dispatch_bless`) stored a caller-provided named argument as-is. So
`class H { has $.x; method y { $!x } }; H.new(x => [1, 2]).y` flattened
under `for` instead of behaving as the itemized Array the constructor
stored, unlike the equivalent default-value case.

All three sites now route the provided value through
`itemize_attr_store_value` like the default-fill loops already did.

Regression test: `t/oo/attribute/self-accessor-decontainerizes.t`.
