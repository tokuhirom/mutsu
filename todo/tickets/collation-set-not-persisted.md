# `$*COLLATION.set(...)` does not persist / is not honored by `coll`

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc` (around line 1420).

## Repro

```
$*COLLATION.set(:quaternary(False), :tertiary(False));
say $*COLLATION.tertiary;
say 'a' coll 'A';
```

- raku: `0` then `Same`
- mutsu: `Nil` then `Less` — the `.set(...)` call has no visible effect on subsequent reads

## Root cause guess

`src/runtime/methods_collection_ops/collation_temporal.rs`'s `"set"` arm calls
`Value::write_back_sharing(&attributes, class_name, new_attrs, id)`, but subsequent reads of
`$*COLLATION` — both the 0-arg `tertiary`/`quaternary` accessor and
`vm/vm_value_helpers.rs::get_collation_settings` (used by the `coll` operator) — don't observe
the update. The write-back likely lands on a different copy/binding than the one later reads
resolve through (a dynamic-variable aliasing issue, similar in shape to other `$*`-dynamic
write-back bugs already fixed elsewhere in the codebase).

## Affected files (starting point)

- `src/runtime/methods_collection_ops/collation_temporal.rs` — the `"set"` method arm
- `src/vm/vm_value_helpers.rs` — `get_collation_settings`

## Suggested next step

Compare how `$*COLLATION`'s instance is stored/retrieved to how another already-working
`$*`-dynamic mutable-attribute case (e.g. `$*TOLERANCE`, if seeded, or any other settable
dynamic singleton) round-trips a `.set`/mutator call, to find where `$*COLLATION`'s path
diverges.
