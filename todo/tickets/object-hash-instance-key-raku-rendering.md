# An object-hash *instance* key renders as `U()` instead of `U.new` in `.raku`

```raku
my class U {}; my %q{Mu}; %q{U.new} = 1;
say %q.raku;   # raku: (my Any %{Mu} = U.new => 1) — mutsu: (my Any %{Mu} = U() => 1)
```

Found while fixing the key parenthesisation
(`news/2026-08/object-hash-raku-parenthesises-keys.md`). Not a
parenthesisation issue and not a general nested-instance issue —
`[U.new].raku` and `(u => U.new).raku` both render `U.new` correctly, and a
custom `method raku` dispatches fine standalone. The divergence is specific
to the object-hash KEY path: `map.typed_key(k)` reconstructs the key from
the stored `.WHICH` representation, and whatever it returns makes
`raku_value` render the `U()` form instead of instance `.new` form (and a
user-defined `method raku { "T.new" }` on the key's class is not
dispatched either, since the fast-path container renderer is a pure
function without an interpreter).

Affected: `src/builtins/methods_0arg/raku_repr.rs` (fast path, via
`object_hash_key_repr` → `raku_value`) and
`src/runtime/methods_native_bypass.rs::dispatch_constrained_hash_raku`
(slow path — this one HAS `&mut self` and could dispatch the key's real
`.raku`). Check how `typed_key` stores instance keys (`original_keys` /
WHICH round-trip) before deciding where the fix goes.

Cosmetic; no known test depends on it.
