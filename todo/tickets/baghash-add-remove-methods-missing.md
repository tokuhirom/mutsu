# `BagHash.add` / `.remove` methods are unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/BagHash.rakudoc:112`).

## Minimal repro

```raku
my $n = BagHash.new: "a", "b", "c", "c";
$n.add('c');
say $n.raku;

$n.remove('a');
say $n.raku;
```

- `raku`: `.add` increments the given key's count by one (or inserts it at count 1 if
  absent); `.remove` decrements it by one (removing the key entirely once its count hits 0).
- `mutsu` (`target/debug/mutsu`): both throw
  `No such method 'add'/'remove' for invocant of type 'BagHash'`.

BagHash already supports the equivalent mutating operations via subscript assignment
(`$n<c>++`, `$n<b> -= 1`, `$n{'a'} = 0` all work correctly per the same doc's next example),
so the underlying mutable-count storage exists — only the named `.add`/`.remove` method
wrappers are missing.

## Root cause hypothesis

`BagHash`'s method dispatch (native `methods_0arg`/`methods_narg`, or the QuantHash-specific
handlers) has no `add`/`remove` arm at all — every other BagHash mutator (subscript
increment/assign, `.roll`, etc.) is implemented, but these two named methods were never
added. Per `raku-doc/doc/Type/Baggy.rakudoc`, `add`/`remove` are documented `Baggy` role
methods, so `Bag`/`Mix` immutable variants correctly don't have them (or throw
immutable-container errors), but `BagHash`/`MixHash` should.

## Affected files (starting point)

- `src/builtins/methods_narg.rs` / `src/runtime/methods.rs` — wherever other BagHash
  mutators (the `$n{'a'} = 0` subscript-store path, `.roll`) are dispatched; `add`/`remove`
  should route through the same per-key count adjustment.
