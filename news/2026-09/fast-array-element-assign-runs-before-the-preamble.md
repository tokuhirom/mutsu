# The plain `@a[$i] = $v` store stops paying for the preamble it never needed

[#8107](https://github.com/tokuhirom/mutsu/pull/8107) gave the positional element store a fast lane
(`try_fast_array_element_assign`) that answers the target's declaration questions from the
container's own embedded metadata instead of re-deriving them by string key. It reduced the store
itself to a `Vec` slot write — but it was wired in at the *bottom* of
`exec_index_assign_expr_named_op`'s dispatch chain, so a plain `@a[$i] = $v` still walked the whole
shared preamble on the way down to it.

That preamble is not small. Before any fast lane is consulted, an element store runs:

- a `Range`-receiver probe against the compiler-baked local slot **and** against env, each cloning
  the value it finds;
- `try_deferred_token_index_assign`, which allocated the variable name as an owned `String` and then
  scanned `code.locals` by name;
- the ADR-0039 unit-lexical cell lookup, plus the env seed/restore it guards;
- `reify_lazy_array_slot`, in case the array is a `LazyList` with a tail to materialize;
- the ADR-0040 rvalue itemization hook (`fetch_proxy_for_store` → `into_deref` →
  `itemize_for_element_store`);
- a `Seq`-destination probe and a `Proxy`-destination probe, which between them resolve the target,
  clone it, and clone the addressed element through any alias cells;
- the two name-keyed cross-thread lanes and the Associative fast lane.

Measured on a `--profile profiling` build by differencing two callgrind runs (a 4,000-iteration loop
with the store against the identical loop without it), that residue was **3,097 instructions and one
heap allocation per store**, on top of a slot write that costs a few dozen.

The observation that makes this fixable is that **every one of those probes asks about a shape the
lane has already refused**: a `Range` receiver, a deferred vivification token, a unit lexical, a
`LazyList`, a `Seq`, a `Proxy`, an aggregate rvalue. They are not questions the store needs
answered; they are questions the *other* stores need answered.

## What landed

`try_fast_array_element_assign_early` — the same lane, consulted **first**, before the preamble
rather than after it. It is a thin wrapper that establishes, on its own, the handful of facts the
preamble would otherwise have established for it:

- `element_share_pending` and `shared_vars_active` are both clear (the `=`-element share is captured
  in the preamble, and the two cross-thread lanes gate on the second flag);
- the subscript is a plain non-negative `Int`;
- the rvalue is a plain scalar, from a deliberate **allow-list** — so `fetch_proxy_for_store` and
  `itemize_for_element_store` are provably the identity and can be skipped. An aggregate rvalue
  itemizes, can be the target itself (`@a[0] = @a` stores a genuinely circular structure), and a
  `Proxy` rvalue must `FETCH`; all three fall through to the unchanged full path;
- `unit_lexical_container_cell` finds nothing for the name (this is *not* the same probe as the
  `unit_lexical_slot` the lane already ran — the cell lookup checks the MAINLINE bucket first, which
  the slot resolver does not);
- the deferred-token probe's by-name slot search cannot find a slot the lane's own dual-store
  coherence check skipped, which is possible only when the baked `target_slot` is out of range for
  the frame.

The wrapper touches nothing — not the stack, not env, not a local slot — unless the lane it calls
commits, so a decline is free and everything downstream runs exactly as it did before.

Separately, `try_deferred_token_index_assign` now borrows the variable name out of the constant pool
instead of copying it. `code` outlives the op and is a distinct borrow from `&mut self`, so the
owned copy bought nothing; it was the last per-store heap allocation #8069 measured.

## Measured

Same method as above, one base (`3f8b9c0a`), `MUTSU_JIT=off`, `target/profiling`:

| per in-range store | before | after |
| --- | ---: | ---: |
| instructions | 3,097 | **1,210** |
| heap allocations | 1.02 | **0.02** |
| `Symbol::intern` calls | 0 | 0 |

Wall clock on the same container, marginal cost isolated by subtracting an identical loop with the
store removed (`target/release`, median of three): **564 ns → 277 ns** per store. rakudo's marginal
cost on this box is roughly 40 ns, so the gap is now ~7x rather than ~14x.

`benchmarks/bench-index-store.raku` goes 0.968 s → 0.829 s against rakudo's 0.715 s here (ratio
1.35 → 1.16), and `bench-threads` 0.776 s → 0.708 s against 0.483 s (1.61 → 1.47). Those two are
local A/B numbers for orientation only — `bench-history.tsv` on the `bench-data` branch remains the
authority for benchmark rows in documents.

## What this does not do

`bench-threads` is still above 1.0, and it will stay there until #8069 §4.2-§4.4 are addressed: the
lane stands down for the whole process once a second mutator thread exists, so the concurrent half
of that row is untouched by anything here. Of #8069's acceptance list, the "**0** `Symbol::intern`
calls and **0** heap allocations per store" half is now met and pinned; the ≤150 ns marginal-cost
half is not (277 ns), and the shared-container scaling bullets belong to the sections still open.

The remaining ~1,210 instructions are no longer preamble. They are the opcode dispatch itself plus
the lane's own guard sequence — roughly twenty checks, several of which are hash probes that a
resolved container descriptor addressed by slot (#8069 §4.1 proper) would collapse into a single
`flags == 0` test. That descriptor is still the right end state; this change makes the cost of not
having it visible, and small enough to measure.

## Pinned by

`t/collections/fast-array-element-assign.t` grows a section for the shapes the early call site now
has to refuse on its own: a module routine's file-scope `@` (new fixture
`t/lib/FastElemUnitLexical.rakumod`, which must not write the loading script's same-named array), a
`Range` receiver, a deferred vivification token, a lazy array whose tail must survive the store, a
`Seq` receiver that writes through its producer's element cells, a marked `=`-element share, and a
concurrent store from four `start` blocks. All 44 assertions in that file pass under rakudo as well.
