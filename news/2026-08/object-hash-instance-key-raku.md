# An object-hash instance key now renders correctly in `.raku`

```raku
my class U {}; my %q{Mu}; %q{U.new} = 1;
say %q.raku;   # (my Any %{Mu} = U.new => 1) now (was: U() => 1)
```

Found while fixing key parenthesisation
(`news/2026-08/object-hash-raku-parenthesises-keys.md`). Not a
parenthesisation issue and not a general nested-instance issue —
`[U.new].raku` and `(u => U.new).raku` both rendered `U.new` correctly
already, and a user-defined `method raku` on the key's class dispatched
fine standalone; the divergence was specific to the object-hash KEY path.

## Root cause

`dispatch_constrained_hash_raku` (`src/runtime/methods_native_bypass.rs`)
renders each pair's VALUE by dispatching the real `.raku` method through the
interpreter (`call_method_with_values(v, "raku", ...)`), which correctly
handles a user-defined `method raku` and an instance's `ClassName.new(...)`
constructor form. The KEY, by contrast, went through
`object_hash_key_repr` → `raku_value` — an allocation-free *pure function*
with no `&mut self`, unable to call into the interpreter at all. For an
instance it fell back to a generic stringification (`U()`), and a
user-defined `method raku` on the key's class was never dispatched.

## Fix

Added `Interpreter::object_hash_key_raku`, mirroring
`object_hash_key_repr`'s Pair.raku-style parenthesisation (a type-object or
Pair key wraps in parens; every other key is bare) but dispatching the
key's own `.raku` through `call_method_with_values` the same way the value
side already does. Used at both call sites in
`dispatch_constrained_hash_raku` (the `Map.new(...)` branch and the general
typed-hash branch).

Pinned by `t/object-hash-instance-key-raku.t`.
