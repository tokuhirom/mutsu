# ASN::META recursive type generation now works

mutsu now passes the actionable `ASN::META` 0.5.3 regression test for
recursive ASN type generation. The interpreter now supports the combination
used by that distribution: roles inheriting from builtin concrete parents,
proto-regex references such as `<value:sym<number>>`, callable `Map(...)`
construction from a lazy pair sequence, dynamic role composition, and the
core `Attribute :rw` trait used by its metamodel.

`ASN::META`'s `t/01-recursive-type.t` passes all three assertions under mutsu.
Its `t/00-sanity.t` remains outside the parity count because Rakudo itself
fails before producing a TAP plan (`This type (Array) does not support elems`).
