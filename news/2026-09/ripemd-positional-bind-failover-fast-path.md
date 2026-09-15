# `Digest::RIPEMD`'s `t/ripemd.t` joins the batteries whitelist

`t/ripemd.t`, the one file in the bundled `Digest` module's own test suite that could not be
whitelisted, was ~11x slower than raku when [#7571](https://github.com/tokuhirom/mutsu/issues/7571)
was filed. A long profiling campaign (worker pool, per-task clone slimming, closure-setup
allocations, reduce compiled-first dispatch, hot-path key memoization) had brought it to ~113.7s —
still inside the batteries gate's hard 120s-per-file `timeout`, but with too little margin for a
slower CI runner to stay whitelisted.

A fresh `callgrind` profile of `rmd160("a" x 4_000)` found two more nameable costs in
`type_matches_value` (`src/runtime/types/type_matching.rs`):

- `binding_signature.rs` runs an implicit `type_matches_value("PositionalBindFailover", &value)`
  check on **every** parameter bind, not just `@`-sigil ones — the flag is computed before the
  sigil is even looked at. `PositionalBindFailover` is a role only `Seq`/`HyperSeq`/`RaceSeq` and a
  user class that explicitly composes it can ever satisfy (`builtin_type_catalog.rs`), so for every
  other value shape (Int, Str, Array, Range, ...) the check walked the whole ~30-branch
  `type_matches` string gauntlet down to the final `dispatch_mro` fallback before answering `false`
  — on every single call. RIPEMD's hot loop (`-> blob32 $h, @words { ... }`, `-> [&f, $r, @K, $s]
  { ... }`) runs this on every round.
- `sub rotl(uint32 $n, $b) { ... }` binds a sized native-int constraint (`uint32`) on every call; the
  existing tag-match fast path only special-cased the bare `Int`/`int` spellings, so a native-width
  constraint fell through to the same slow gauntlet before reaching `is_native_int_type`'s arm near
  its end.

Both are now short-circuited directly in `type_matches_value`'s existing fast-path block (the same
place the `Any`/tag-match fast accepts already live, gated on the subset registry the same way, so a
user `subset PositionalBindFailover of ...` or a native-int type name still falls through to the
full checker). Neither fix is RIPEMD-specific — both sit on the generic parameter-binding path.

**Measured** (release, this session's 4-core container): the gate proxy `rmd160("a" x 100_000)`
dropped ~7.0s → ~6.1s, and the full upstream `t/ripemd.t` dropped **113.7s → ~73s** (9/9 pass) — a
~36% wall-clock reduction and real margin under the 120s budget. Pinned by
`t/routines/signature/array-param-positional-bind-failover.t` (Array/List/Range/Seq/HyperSeq still
bind correctly to an `@`-sigil parameter, and Int/Str/Hash still correctly fail to). `Digest`'s test
suite is now fully whitelisted — see `docs/batteries/digest.md`.

A pre-existing, unrelated bug surfaced while writing that regression test — a class that explicitly
composes `PositionalBindFailover` does not actually get its `.cache` method invoked when bound to an
`@`-sigil parameter — and was filed separately as
[#8456](https://github.com/tokuhirom/mutsu/issues/8456) rather than folded into this fix.
