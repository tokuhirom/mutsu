# An `is Array` subclass read through a variable does not delegate its rendering methods

```raku
class R is Array {}
my $v = R.new(1, 2);
say $v.Str;          # raku: "1 2"    mutsu: "R()"
say R.new(1, 2).Str; # raku: "1 2"    mutsu: "1 2"   (correct)
```

Measured 2026-09-04 against `raku` v2026.06, on `main` plus
`news/2026-09/builtin-subclass-type-identity-and-constant-class-alias.md`.

## Why

An `is Array`/`is List` subclass keeps its elements in a backing
`__mutsu_array_storage` attribute, and the two method-call opcodes delegate to
it differently:

- `OpCode::CallMethod` (`vm_call_method_ops.rs`, the receiver-less/chained form)
  delegates **by default** — everything the class does not define itself, minus
  the type-identity exclusion `Interpreter::is_type_identity_method` added with
  that news entry.
- `OpCode::CallMethodMut` (`vm_call_method_mut_ops.rs`, the through-a-named-
  variable form) delegates through an **allowlist**,
  `Interpreter::is_array_storage_native_safe`, which carries the list protocol
  (`sort`/`reverse`/`elems`/`AT-POS`/`head`/…) but none of the renderers.

So `.Str` falls through to ordinary `Instance` stringification and answers
`R()`. `.gist` and `.raku` happen to answer `[1 2]` / `[1, 2]` through a
different fallback, so `.Str` is the visible one — which is why the two paths
disagree only here.

## What to do

Do not simply append `"Str" | "gist" | "raku" | "perl" | "Bool" | "Numeric" |
"Int"` to `is_array_storage_native_safe` without measuring: that list is
documented as "non-mutating, non-rw-view list methods that are safe to dispatch
on the backing storage via `try_native_method`", and its exclusions
(`map`/`first`/`grep`/`minmax`, the mutators) are deliberate. The better shape is
probably to make the two opcodes share ONE decision — the Associative twin
already has a single `is_hash_storage_method` allowlist used from both sides —
rather than keeping an allowlist on one path and a denylist on the other.

Check the `.raku` itemization divergence at the same time: raku answers
`$[1, 2]` for `R.new(1,2).raku` (the `$` marks the itemized subclass instance)
and mutsu answers `[1, 2]` in both paths. That is a separate, pre-existing gap,
but it lives in the same delegation.

## Repro

The three lines above, no fixtures. `t/builtin-subclass-type-identity.t`
asserts the chained `.Str` (test 11); add the through-a-variable spelling to it
when this is fixed.
