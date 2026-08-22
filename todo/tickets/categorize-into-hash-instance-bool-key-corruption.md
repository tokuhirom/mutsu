# `.categorize`/`.classify` into an untyped `Hash` mis-renders a non-Str bucket key when the source array holds class instances

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc` (around line 961).

## Repro

```
my %h;
class Foo {}
my @a = (Foo.new,);
@a.categorize({True}, into => %h);
say %h;
```

- raku: `{True => [Foo.new]}`
- mutsu: `{Bool|1 => [Foo.new]}` — the `True` bucket key renders as its internal `WHICH`-style
  representation instead of `True`

The identical script with `@a = (1,)` (a plain Int element instead of a class instance) prints
correctly: `{True => [1]}`.

## Investigation so far

`dd %h` shows the *underlying* hash structure looks identical in both cases (`%{Any} =
Bool::True => ...`, `which_keyed=false`, `key_type=Some("Any")` via `rust-gdb` breakpoints in
`classify_finish_hash`/`ensure_object_hash_which_keys` in
`src/runtime/builtins_collection_classify.rs`) — so the corruption is not visible at
construction time, only when the result is rendered via `say`/`.Str`/`.gist`, and only for the
instance-array run.

`typed_key()` in `src/value/value_collections.rs` has a fallback that stringifies the raw
`.WHICH` key when a lookup misses — consistent with the observed `Bool|1` output — but it's
unclear why the lookup misses only when the source array's elements are `ValueView::Instance`.

## Affected files (starting point)

- `src/runtime/builtins_collection_classify.rs` — `classify_finish_hash`,
  `ensure_object_hash_which_keys`
- `src/value/value_collections.rs` — `typed_key()` and its miss-fallback

## Suggested next step

A further `rust-gdb` session watching the `Gc<HashData>`'s `original_keys` field between the
write-back call and the final `say`/gist render, to see whether something in instance
GC-tracking touches that allocation in between (the divergence only appears with an
`Instance`-holding source array, which hints at a GC-interaction bug rather than a classify
logic bug).
