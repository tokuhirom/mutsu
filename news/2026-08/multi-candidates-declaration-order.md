# `Routine.candidates` now returns multi candidates in declaration order

`Routine.candidates` must return a `multi` sub's candidates in DECLARATION
order (Rakudo does). mutsu returned them in an order that depended on Rust
`HashMap` bucket layout — and worse, that order was unstable against
unrelated statements elsewhere in the file (adding/removing unrelated code
before the multi declarations could flip the candidate order). Because the
order was wrong, calling a candidate positionally by index could pick the
wrong one: `&mm.candidates[0].(7)` could die with a type error because index
0 was not actually the `Int` candidate.

## Root cause

Each `multi` candidate is registered TWICE at runtime: once by the
forward-declaration/hoist pre-pass (`Compiler::hoist_sub_decls`, which walks
a block's statements top-to-bottom and stamps every candidate's `decl_order`
in true declaration order), and once by the in-sequence pass that runs when
execution actually reaches the statement. The second registration cannot
reuse the hoisted registry key — candidates are keyed by mangled type
signature (`GLOBAL::mm/1:Int`), which the hoist pass already occupied — so it
falls back to a `__m{N}`-suffixed key. That leaves two registry rows per
candidate with the same body (`body_fingerprint`) but different `decl_order`
stamps.

`Interpreter::routine_candidate_subs`
(`src/runtime/methods_signature_candidates.rs`) scanned `registry.functions`
for keys matching the routine name and deduped by body fingerprint, keeping
whichever row the `HashMap` iteration visited first — arbitrary bucket order,
unrelated to declaration order and unstable against unrelated code elsewhere
in the file.

## Fix

The reader-side fix turned out to be sufficient on its own — no change to
registration was needed. `routine_candidate_subs` now collects every matching
registry row (both copies of each candidate), sorts them by `decl_order`
first, and only then dedupes by body fingerprint, keeping the smallest
`decl_order` per fingerprint. That is always the hoist-pass row, since
hoisting stamps candidates in true source order chronologically before any
in-sequence stamp — so the surviving order is deterministic and matches
declaration order regardless of `HashMap` bucket layout. This mirrors the
established `decl_order` min-per-key dedup pattern already used for
token/grammar proto candidates (`token_key_decl_order`,
`sort_sym_keys_by_decl_order` in `runtime/resolution.rs`).

The `__mutsu_multi_index` the scan assigns for doc-comment lookup inherits
the corrected order automatically, since it is assigned positionally over
the now-correctly-ordered result.

Pinned by `t/multi-candidates-declaration-order.t`, which checks declaration
order is preserved across several candidate type orderings, that it is
stable against unrelated leading statements (the actual regression the bug
report described), and that `.candidates[N]` now dispatches to the
N-th-declared candidate.
