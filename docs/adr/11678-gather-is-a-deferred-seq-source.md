# ADR-11678: A gather is a deferred `SeqSource`

- **Status**: Accepted — steps 0-3 implemented (2026-10-12, see §8); the hand-off retirement, step 4 and step 5 remain (#13026); the lazy Slip of a genuinely lazy list implemented, a gather's lazy Slip built as a bounded-pull interim (§8.6, #13117, #14295)
- **Date**: 2026-10-07
- **Deciders**: tokuhirom, Claude
- **Amended**: 2026-10-12 (see "Amendment" below)
- **Addresses**: [#11678](https://github.com/tokuhirom/mutsu/issues/11678)
- **Related**: [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
  (the `SeqBody` reification and consumption contract; §2.5 and §6 leave `LazyList` out of
  scope, §3.5 is why `SeqBody` is an `Arc`), [ADR-0058](0058-map-grep-produce-a-deferred-seq.md)
  (deferred producers live in `SeqSource`; §8 lists the read paths that had to learn about one),
  [ADR-0119](0119-seq-sources-pulled-a-prefix-at-a-time.md) (prefix pulls; §1 measured what a
  `LazyList`-hosted Seq breaks), [#11910](https://github.com/tokuhirom/mutsu/issues/11910) and
  its fix [#12366](https://github.com/tokuhirom/mutsu/pull/12366) (literal gathers stay lazy;
  render-time reification of a held gather)

## Amendment (2026-10-12)

The 2026-10-07 draft described the symptom as "a gather nested in a collection renders its empty
seed", proposed a `SeqSource::Gather` that "retains the captured gather body state", and left
open whether the coroutine itself moves into `SeqBody`. Since then #12366 (2026-10-08, closing
#11910) removed the empty-seed rendering, and a re-measurement found the remaining divergences
are about itemization, identity, consumption and resuming after a prefix, not rendering. This
amendment:

- replaces §1 with the re-measured state (§1.2) and records why ADR-0034 excluded `LazyList` in
  the first place (§1.4);
- decides the open question: `SeqSource::Gather` is an **adapter** over the existing
  `Gc<LazyList>`; the coroutine does not move (§2, §4);
- limits the first slice to plain `gather` (§2.3), makes streaming a requirement (§2.4), adds an
  implementation plan with a size and risk estimate (§5), and makes the acceptance evidence
  measurable (§7).

The 2026-10-07 text is in this file's git history.

## 1. Context

### 1.1 What #12366 already fixed

`(1, gather { for 1..* { take $_ } })` and `(:a(gather {...}), 5)` construct without running the
body, and `$l[1][^3]` / `$p[0].value[^2]` read a prefix. A finite gather inside a list renders its
elements: `needs_raku_dispatch` (`src/value/raku_repr.rs`) sends `.raku` on an unforced gather
coroutine through method dispatch, which reifies it, and `reify_nested_map_grep_for_read`
(`src/vm/vm_helpers_lazy_nested.rs`) has a `LazyList` arm that fills a finite nested gather for
the pure readers. Pinned by `t/collections/lazy-seq/gather-in-list-literal-lazy.t`.

Both additions are `LazyList`-specific forcing paths at readers. They fixed what they target and
nothing else, because the rules that are still missing (below) are rules `SeqBody` already has.

### 1.2 Remaining divergences (measured 2026-10-12)

mutsu debug build of `9b4587e0`, Rakudo v2026.09. `$g` is `my $g = gather { take 1; take 2 }`
unless the row says otherwise.

| # | Expression | mutsu | raku |
| --- | --- | --- | --- |
| **Itemization** | | | |
| 1 | `my $g = gather { take 2 }; ("a", $g).raku` | `("a", (2,).Seq)` | `("a", $((2,).Seq))` |
| 2 | `my $g = gather { take 2 }; $g.raku` | `(2,).Seq` | `$((2,).Seq)` |
| 3 | `my %h = a => (gather { take 5 }); (%h<a>:kv).raku` | `("a", (5,).Seq)` | `("a", $((5,).Seq))` |
| 4 | `my $g = gather { take 1 }; (:k($g)).raku` | `:k((1,).Seq)` | `:k($((1,).Seq))` |
| 5 | `my $g = gather { take 1 }; [$g].raku` | `[(1,).Seq]` | `[(1,).Seq,]` |
| 6 | `($g, 2).flat.elems` | `3` | `2` |
| 7 | `my @a = $g; @a.elems` | `2` | `1` |
| **Identity** | | | |
| 8 | `$g === $g` (also with `my $g := gather ...`) | `False` | `True` |
| 9 | `my $h = $g; $g === $h` | `False` | `True` |
| 10 | `$g.WHICH eq $g.WHICH` | `False` | `True` |
| **Consumption** | | | |
| 11 | `$g.head(1); $g.List` | `(1)` | `X::Seq::Consumed` |
| 12 | `$g.List; $g.List` | `(1 2)` twice | `X::Seq::Consumed` on the second |
| 13 | `my $i = $g.iterator; $i.pull-one; $g.List` | `(1)` | `X::Seq::Consumed` |
| 14 | `my $h = $g; $h.List; $g.List` | `(1 2)` | `X::Seq::Consumed` |
| **Resume after a prefix (wrong answers)** | | | |
| 15 | `$g[0]; $g.List` | `(1)` | `(1 2)` |
| 16 | `?$g; $g.List` | `(1)` | `(1 2)` |
| 17 | `my $g = gather { for 1..3 { say "t$_"; take $_ } }; say $g[0]; my $h = $g; say $h.List; say $g.List` | `t1 1 (1) (1)` | `t1 1 t2 t3 (1 2 3) (1 2 3)` |

Rows 1-3 are the three cases of the original draft; 4-17 were not in it. Rows 15-17 are wrong
answers, not just leniency.

Already matching, and must stay so: an infinite gather inside a list, Pair or Hash constructs
without running and answers `[^n]`; `$inf.head(3)`; `$inf[^3]` then `$inf[5]`;
`for gather { ...say "t$_"; take $_ } -> $x { say "b$x"; last if $x >= 2 }` and the same over
`$g<>` print `t1 b1 t2 b2`; `$inf.iterator.pull-one` twice runs the body twice; `$g.cache`
then `.List` twice; `$g.elems` then `.List`; `@$g` twice; `$g eqv $g`; `die` in the body under
`try`; `my @a = gather {...}` is `[1, 2]`; `.^name` is `Seq` and `.is-lazy` is `False`;
`lazy gather { take 1 }` renders `$((1).lazy.Seq)`. A plain infinite gather assigned to an `@`
variable hangs in both, because a plain gather is not `.is-lazy`.

Not gather-specific, so out of scope here (to be filed separately): `sub f(*@a) { @a.elems };
f($s)` is `2` in mutsu and `1` in raku for any `$`-held Seq, `(1,2).Seq` included; other
`LazyList` kinds share the identity rows (`my $s = (1...*); $s.WHICH eq $s.WHICH` is `False`,
and `$s === $s` dies with "Cannot coerce an infinite lazy list" where raku says `True`).

### 1.3 Why these are one defect

Every row is a rule `SeqBody` implements and `LazyList` does not:

- **Itemization** is `SeqView::ItemSeq`, a handle tag over a shared core. `LazyList` has only an
  `itemized` flag, which exists for the sink exemption and which no `.raku`, `.flat` or `@`
  assignment path reads.
- **Identity** is `SeqCore::which_id`, shared by every handle. A `LazyList` has none that
  survives a read, and `$s = SEQ` replaces the node: `LazyList::with_itemized`
  (`src/value/value_lazy.rs`, called from `vm_var_assign_set_local.rs`) clones the whole list,
  including its cache and its suspended coroutine state, into a new `Gc`.
- **Consumption** is `seq_method_consumes` + `SeqBody::take` on a core every alias shares. A
  cloned `LazyList` has nothing shared to mark consumed, and `LazyList` has no consumption state
  of its own.
- **Resume after a prefix** is `SeqBody::pull_prefix` storing the prefix into the body and
  leaving the source in place. Row 17 shows the coroutine clone forking: `$h` and `$g` are two
  independent copies of one suspended body.

### 1.4 History of the exclusion in ADR-0034 §2.5

ADR-0034 (2026-08-19/20) targeted the `LazyIoLines` `.defined` / destroy-on-materialize bug.
Its §1.5 lists `LazyList` as the third Seq representation and marks it out of scope; §6 splits
the `LazyList` forcing question off as a different defect ("where forcing happens", not "what
forcing does to a Seq"); §2.5 accordingly says `LazyList` "is untouched. See §6." That was a
scope decision for one ticket, not a technical prohibition.

The constraint that looks technical, §2.1's "a `SeqBody` reader cannot invoke the VM" (`Deref`
returns the live generation and never pulls), applies to every deferred source. ADR-0058
(proposed 2026-08-22, shipped 2026-09-07) met it for `.map`/`.grep` with an
`Interpreter::pull_seq_source` arm reached from the dispatch points, plus a bounded list of
read-path fixes (ADR-0058 §8.2). ADR-0119 added prefix pulls on the same body. Nothing about
that constraint is specific to gather, so the scope reason has lapsed.

One real constraint remains, ADR-0034 §3.5: `SeqBody` is behind an `Arc`, not a `Gc`, because a
`Seq` is not a cycle participant, with "revisit only if a measured Seq-participating cycle
appears". A gather is one: it captures an `Env` and its body can name the variable that holds
the gather. Today that state lives in a `Gc<LazyList>` node (`impl Trace for LazyList`,
`src/value/value_gc.rs`): `trace` walks the captured env overlay (when uniquely owned), the merged
capture, `cache`, `generation_state` and `elems_count`, and `drop_gc_edges` severs them. The
suspended coroutine's own `locals`/`stack`/`env` are not traced today; the file documents this as
a conservative gap (a cycle through them is not reclaimed, never wrongly freed). Moving that
state into an `Arc<SeqBody>` would put it outside any node: `SeqBody`'s edges are traced only
while its `Arc` is uniquely owned (`uniquely_owned` in `value_gc.rs`), and the body has no
`drop_gc_edges`, so a cycle through it could never be collected.

## 2. Decision

### 2.1 An adapter source

Represent a plain `gather` as a `Seq` whose `SeqBody` has a new source

```rust
/// A `gather` body not yet run to completion: the existing `Gc<LazyList>`
/// gather coroutine, held as one `Value` edge. The coroutine, its GC tracing
/// and its resume machinery stay in `LazyList`.
SeqSource::Gather(Value)
```

`exec_make_gather_op` builds the `LazyList` exactly as today and returns
`Value::seq_deferred(SeqSource::Gather(Value::lazy_list(gc)))`. `LazyList` keeps the coroutine
(`GatherCoroutineState`), the compiled body, the captured env, the unit/package/`samewith`
restoration, `running_collector`, `for_loop_resume` and the take-limit suspension.
`force_lazy_list_vm` / `force_lazy_list_vm_n` remain the only gather drivers; the new
`pull_seq_source` arm and the prefix and streaming arms call them at dispatch points on the
interpreter already running.

`SeqBody` then owns what Raku attaches to a `Seq`: identity (`which_id`), aliasing (one core per
gather, shared by every handle), consumption (`seq_method_consumes` / `take` /
`X::Seq::Consumed`), `.cache` (`cache_requested` / `retained`), `$` itemization
(`SeqView::ItemSeq`) and the sink exemption (`SeqState::itemized`).

### 2.2 Invariants

1. **The inner `LazyList` is private to the adapter.** It is never handed to user-visible code
   as a `Value::LazyList`; otherwise there are two identities again. The exceptions are the
   steals in §2.3, where Raku also consumes the Seq.
2. **Elements a reader sees come from the body's generations.** The `LazyList` cache stays the
   coroutine's own buffer, read by `running_collector` and the `@a := gather { ... @a ... }`
   self-reference. A pull stores what it produced into `SeqBody`.
3. **A gather pull does not mark the body `Taken` while it runs.** `pull_and_store` swaps the
   source for `Taken` during a pull; for `MapGrep` that is right. For a gather it would turn the
   supported self-reentry (`my \S := gather { ... S.iterator ... }`,
   `t/collections/lazy-seq/gather-self-reference-iterator.t`) into `X::Seq::Consumed`. The
   `Gather` arm therefore clones the `Gc` handle out under the state lock, releases the lock,
   and applies the `Reified`/`Taken` transition after the pull. The `SeqCore` state lock is never
   held across a pull.
4. **GC.** `SeqSource::trace_edges` yields the `Value` edge, so the `LazyList` node is reached
   whenever the body is uniquely owned. When several holders share the `Arc`, the edge acts as
   an external root: conservative, never a wrong free, but a cycle through an aliased gather may
   be collected later than today (today an alias is a separate cloned node). Step 0 (§5) pins
   the current reclaim behaviour so this is measured, not assumed.
5. Construction never runs the body; no `Interpreter` is constructed for a pull.

### 2.3 First slice: plain `gather` only

| Feature | First slice |
| --- | --- |
| `gather { }` / `gather STMT` (`Expr::Gather`, `compiler/expr.rs`) | `SeqSource::Gather` |
| `lazy gather`, `.lazy` on a gather (`__mutsu_preserve_lazy_on_array_assign`, 14 refs) | Stays `LazyList`. `.lazy` on an untouched `Seq(Gather)` steals the inner list (Raku's `.lazy` consumes the Seq) and marks it as today. |
| `lazy for` (`compiler/expr.rs`, also `MakeGather`) | Stays `LazyList`; `MakeGather` gets a mode operand (`OpCode` stays at most 48 bytes). |
| `@`-variable lazy element store (`array_context`, 73 refs) | Stays `LazyList`. `my @a = gather {...}` reifies at the `@` assignment, as ADR-0034's assignment site already does for any Seq. |
| `.map`/`.grep` over a gather (`lazy_pipe` with a gather upstream, `is_lazy_pipe_source`) | Steals the inner list and builds the existing `lazy_pipe` over it (Raku's `.map` consumes the Seq). Folding it into `MapGrepItems::Chain` is a later slice. |
| `running_collector` self-reentry | Unchanged in `LazyList`; reached because of invariant 3. |
| `for_loop_resume`, `vm_take_deferring_op` (`@a».take`), `vm_try_catch_gather` (`take` through `try`/`CATCH`) | Unchanged: they are coroutine suspension internals reached through `force_lazy_list_vm_n`. |
| `LazyList::itemized`, `__mutsu_lazylist_from_gather` (7 refs) | Unused by a plain gather once it is a Seq; kept for the `LazyList` kinds above. |

### 2.4 Streaming is mandatory

A `Seq(Gather)` that can only be pulled whole would hang every infinite gather the table in
§1.2 lists as matching. The slice therefore lands with:

- **`for`**: claim the source for the loop and pull one `take` per iteration through the existing
  gather loop driver (`vm_for_loop_lazy.rs`), handing the pulled elements back afterwards, as
  `claim_map_grep_for_streaming` / `finish_map_grep_streaming` do for `MapGrep`
  (`src/value/seq_body/map_grep_stream.rs`). The body and `take` interleave, and `last` stops
  the body.
- **`.iterator`**: steal the source into an iterator that resumes the coroutine per `pull-one`
  (the `take_map_grep_stream_source` shape).
- **Prefixes**: `take_prefix_source` gets a `PrefixSource::Gather` (consuming `.head(n)`,
  `.first`), and `pull_prefix` gets a `Gather` arm (non-consuming `[i]`, `[^n]`, `?$g`) that
  stores the prefix into the body and leaves the source in place. That is rows 15-17.
- **Pure readers**: `awaits_vm_reify` includes `Gather` (`needs_touch` already answers `true` for
  any unreified source). `has_deferred_source` does **not**, for the reason ADR-0058 gives for
  `MapGrep`: its callers read it as "single-use, steal it", while `@$g` twice must keep working
  in Raku. Readers that cannot reach the interpreter (`Deref`, the pure `raku_repr` renderer,
  `Value::eqv`, `Value::truthy`, `.gist`, the infallible set operators of ADR-0058 §8.4) see the
  empty seed of an unpulled body; each is audited, and the ones that test
  `is_map_grep_source()` move to `awaits_vm_reify()`. ADR-0058 §7 calls this read path
  load-bearing.

## 3. Consequences

- Rows 1-17 of §1.2 follow from `SeqBody`'s existing rules; no gather-specific itemization,
  identity or consumption code is written.
- mutsu becomes stricter: rows 11-14 raise `X::Seq::Consumed`. Code (local tests included) that
  relied on re-reading a consumed gather breaks; roast is the arbiter.
- The `LazyList` arms #12366 added become redundant for plain gathers (§5 step 4).
- Two lazy-sequence mechanisms remain for gathers until the later slices: `Seq(Gather)` for
  plain gathers, `LazyList` for `lazy gather`, `lazy for` and the `@` element store.
- Follow-up outside this file: once Accepted, ADR-0034 §2.5 gets a pointer here (Status suffix
  or amendment), without rewriting its decision. ADR-0058 §6's "pull-granular deferred Seq"
  follow-up is partly answered by §2.4.

## 4. Rejected approaches

- **Force every gather found in a collection.** Finiteness cannot be known before the body runs;
  constructing a collection with an infinite gather would hang.
- **Special cases in Pair/Hash/list rendering.** Duplicates the forcing protocol per consumer
  and leaves aliases and other readers with the empty seed.
- **Keep `LazyList` and force at the renderer.** #12366 implemented part of this (the
  `needs_raku_dispatch` and nested-reify arms). It fixed the empty-seed rendering, and it is a
  stopgap: it keeps two reification, consumption and aliasing systems for one Raku type, and it
  cannot reach rows 6-17, which are not rendering. ADR-0119 §1 measured the same failure
  families (`X::Seq::Consumed` after `.head`, the `$( )` of an itemized Seq) for a
  `LazyList`-hosted Seq.
- **Teach `LazyList`'s own `itemized` flag to the `.raku`/`.flat`/`@`-assignment paths.**
  Would fix rows 1-7 at best, as a second itemization mechanism next to `SeqView::ItemSeq`.
  Because `with_itemized` clones the node, rows 8-17 stay. A band-aid.
- **Give `LazyList` the rest of the Seq rules** (an identity, a shared consumption flag, a prefix
  store). Rebuilds each `SeqBody` rule a second time; rejected by ADR-0119 §4 for the same
  reason.
- **Move the coroutine (`GatherCoroutineState`, env, locals, stack) into `SeqBody`.** Rejected
  for now: it takes gather state out of the `Gc` graph (§1.4), and the adapter gives every
  rule in §1.2 without it. If a later step moves it, ADR-0034 §3.5's re-evaluation trigger is
  met, and that step needs its own amendment deciding how `SeqBody` (or the part of it holding
  the coroutine) joins the `Gc` graph.

## 5. Implementation plan

| # | Step | Notes |
| --- | --- | --- |
| **0** | **Pin Rakudo's behaviour first.** `t/collections/lazy-seq/gather-seq-body-oracle.t` (the path `scripts/migrate-t-layout.py --where` gives), every row checked against `raku`: rows 1-17 (as `todo`), the "already matching" list of §1.2, an infinite gather in a list/Pair/Hash, resume after a prefix, aliases, `.cache`, consuming reads, scalar itemization, `Slip`/`.flat` (`take \|(2,3)`), `take` of a nested gather, exceptions and control flow (`die` under `try`, `return` in the body targets the routine that wrote it, `last`/`next` in a `for` inside the body), and GC of an unforced self-referencing gather (DESTROY count in a child process, in the style of `t/vm/destroy-snapshot-gc-counts.t`; record today's count). Run the exposure probe ADR-0058 §8.1 recommends: the real mechanism behind a throwaway env gate, over `t/` and the roast whitelist. | Separate PR. Changes no behaviour. |
| **1** | **Adapter source**: `SeqSource::Gather`, its `trace_edges` and `pull_seq_source` arms, invariant 3, `MakeGather` mode operand, the `.lazy` / `.map` / `.grep` steals of §2.3. New code goes in `src/value/seq_body/gather_source.rs` (`seq_body.rs` is already 1421 lines). | Lands with 2 and 3. |
| **2** | **Streaming** (§2.4): `for`, `.iterator`, `take_prefix_source`, `pull_prefix`, `awaits_vm_reify`. | Without it, infinite gathers hang. |
| **3** | **Reader migration**: each site that reaches a plain gather through `ValueView::LazyList` / `is_gather_coroutine` / `__mutsu_lazylist_from_gather` is checked; the pure-reader audit of §2.4. | Driven by the step-0 probe list. |
| **4** | **Retire the #12366 stopgaps** for plain gathers: narrow the `needs_raku_dispatch` and `reify_nested_map_grep_for_read` `LazyList` arms to the gather kinds still on `LazyList`, and delete them once none is. | |
| **5** | Later slices, each with its own oracle rows: `lazy gather` and `lazy for` (`SeqState::lazy`), the `@` lazy element store, `MapGrepItems::Chain` over a gather upstream. | Out of this ADR's first slice. |

Steps 1-3 land as one PR: a `Seq(Gather)` without streaming or reader coverage is a regression,
not a slice.

### 5.1 Size

Counted with `git grep` on `9b4587e0`:

- `ValueView::LazyList`: 295 matches in 129 files (all `LazyList` kinds; only the ones a plain
  gather reaches are in scope).
- `force_lazy_list_vm*` calls: 86 in 56 files (59 `force_lazy_list_vm(`, 23 `_n(`,
  4 `_window(`).
- `.coroutine`: 39 references in 21 files; `is_gather_coroutine`: 2; `for_loop_resume`: 53;
  `running_collector`: 11.
- `ValueView::Seq`: 501 existing matches, whose rules the gather inherits.
- Tests mentioning `gather`: 182 `t/` files and 62 whitelisted roast files.

Step 1 and 2 are bounded (one source variant plus arms mirroring `MapGrep`'s). Step 3 is the
unknown: ADR-0058's first full `t/` run under the real mechanism showed 44 failures, converging
to 0. Expect the same order.

### 5.2 Risks

- **Roast turning red from stricter consumption** (`.head` then `.List`, `.List` twice, a
  consumed alias). Fixed forward; a whitelisted file is never dropped, and a local `t/` test that
  pinned the old leniency is changed to Rakudo's behaviour.
- **Laziness regressions that hang**: any reader that reifies a `Seq(Gather)` whole where
  `LazyList` pulled incrementally. Step 0's infinite-gather rows run under a timeout.
- **GC leak regressions**: invariant 4. Measured by the step-0 DESTROY test.
- **Reentrant gathers and control paths**: self-reference (`running_collector`,
  `GatherSelfRef`), `return`/`samewith`/package and unit restoration, the CATCH marker,
  take-limit suspension through `try`. All of these live in `force_lazy_list_vm*`, so they are
  kept as long as every pull goes through it.
- **Double element storage**: the `LazyList` cache and the body's generations hold the same
  elements. Measure the memory of a large drained gather; drain the inner cache after a full
  pull if it matters.

### 5.3 Rules that apply

No new slow paths: pulls run the compiled gather body through `force_lazy_list_vm*`, never
through `call_method_with_values` / `run_instance_method`. Never build an `Interpreter`. Every
new `SeqBody` method, pull arm and changed opcode handler gets a `// Cost:` line. A primitive has
one implementation: `force_lazy_list_vm*` is the only gather driver; no second resume loop.
`size_of::<OpCode>()` stays at most 48 bytes. `check-layer-deps`: `src/value/` must not name
`vm`/`runtime`; the `Gather` pull arms live in `src/vm/`.

## 6. Open questions

Accepted on 2026-10-12 as the design direction. The two questions below are settled in the
implementation PRs (the first when step 3's probe shows which files change, the second in
step 0's DESTROY measurement), not before:

- Whether rows 11-14 (stricter consumption) land in the same PR as rows 1-10 and 15-17, or behind
  a separate roast sweep.
- Whether the GC behaviour of invariant 4 (an aliased gather's cycle collected later) is
  acceptable, or the slice must wait for `SeqBody` to join the `Gc` graph.

## 7. Acceptance evidence

Before this ADR is marked implemented:

1. `t/collections/lazy-seq/gather-seq-body-oracle.t` passes with no `todo`, rows 1-17 included,
   every expectation checked against `raku`.
2. Constructing a list, Pair and Hash holding an infinite gather does not run the body (a side
   effect counter stays 0), and `[^3]`, `.head(3)`, `for ... { last }` and two
   `.iterator.pull-one` calls run the body exactly as many times as Rakudo (counter asserted).
3. A prefix read followed by `.List` returns the full sequence (rows 15-17).
4. The DESTROY count of the step-0 GC test is at least the count recorded before the change.
5. The full gate (`scripts/dev gate --full`: `make test` + `make roast`) is green with no
   whitelist removal; changed local tests are listed in the PR with the Rakudo output that
   justifies each.
6. No new `ValueView::LazyList` arm is added for plain gathers, and the #12366 arms are narrowed
   or deleted (step 4).
7. Gather creation plus full drain is benchmarked before and after (the `perf-tuning` skill's
   A/B procedure), and the ratio is recorded here.

The implementation uses compiler/VM execution paths only: no collection-specific rendering
hacks and no eager forcing.

## 8. Implementation record (2026-10-12)

Steps 0-3 shipped: step 0 as its own PR (#13005), steps 1-3 together. What the
code does where §2 left a choice, and where it deviates:

- **The adapter** is `SeqSource::Gather(Value)` (`src/value/seq_body.rs`, the
  gather-specific `SeqBody` methods in `src/value/seq_body/gather_source.rs`,
  the interpreter half in `src/vm/vm_gather_seq.rs`). `MakeGather` carries a
  `GatherMode` operand: `Seq` for `Expr::Gather`, `LazyList` for `lazy for`.
- **Prefix pulls cache the Seq.** Rakudo's `[i]` and `Bool` cache a Seq (`$g[0];
  $g.List; $g.List` and `?$g; $g.List; $g.List` both answer twice), so
  `SeqBody::extend_gather_prefix` sets `cache_requested`. Because the inner
  list keeps its own cache of every element taken, a prefix pull stores the
  whole prefix the driver returns rather than appending.
- **`.List` / `.list`** on an uncached gather steal the inner list into a fresh
  cached body and return its List view (`take_gather_as_list`), so `for
  $g.list` still interleaves with the body and `(gather { loop {...} }).list[2]`
  answers. On a cached gather they return a List view over the same core.
- **Steals.** `.lazy`, `.map`, `.grep`, `.first` and `.iterator` hand the inner
  list on, consuming the Seq (§2.3). On a cached gather `.map`, `.grep`,
  `.first` and `.iterator` share the inner list instead, which is how a body
  that names its own gather (`my \S := gather { ... S.iterator ... }`, read by
  `S[^3]`) gets an iterator mid-pull (invariant 3).
- **Deviation from invariant 1: operand and argument boundaries.** The zip and
  cross metaoperators and reductions take a gather operand as its inner list
  (consuming the Seq, as Rakudo's iterating operators do). Built-in routines
  (`call_function_arms`, `try_native_function`), methods of built-in receivers
  (`call_method_with_values_unviewed`, skipped for a method the program
  declared) and `.AT-POS` / `.EXISTS-POS` receive the inner list *without*
  consuming the Seq. Those consumers already stream a `LazyList`, and rewriting
  each of them to pull a `SeqBody` is the reader migration this slice did not
  attempt; the cost is leniency (an iterating builtin does not consume the
  gather) and a second identity reachable inside those builtins. Retiring the
  boundaries one consumer at a time is follow-up work (#13026).
- **Builtin routines no longer share the inner list** (#13026 step 1, §8.2),
  nor do the methods of built-in receivers (step 2, §8.3) or `.AT-POS` /
  `.EXISTS-POS` (step 3, §8.4).
- **Pure renderers.** `say`/`put`/`print`/`note`, a `--> Array(...)`-style
  return coercion, slurpy binding and `.flatmap`'s flattening run a gather
  whole first (`reify_gather_seq`), as Rakudo's do; nested gathers are run by
  `reify_nested_map_grep_for_read` for the `.raku`/`.gist`/`.Str`/`.flat`
  readers. A List literal no longer reifies a gather element at construction.
- **#13003 fixed on the way.** The gather body is recompiled for forcing; the
  recompile now keeps the definition site's `lexically_in_routine`
  (ADR-0050), so a `return` in the body reaches the still-running routine.
- **Step 4 not done.** The #12366 `LazyList` arms (`needs_raku_dispatch`,
  `reify_nested_map_grep_for_read`) are still reached by `lazy gather`, `lazy
  for` and the stolen inner lists, so they stay until step 5.

### 8.1 Evidence (§7)

1. `t/collections/lazy-seq/gather-seq-body-oracle.t`: 60 tests, no `todo`,
   rows 1-17 included; `raku` passes the same file.
2. Construction of a List, Pair and Hash holding an infinite gather runs the
   body 0 times; `.head(3)` runs it 3 times, two `.iterator.pull-one` twice
   (asserted in the oracle).
3. Rows 15-17 answer the full sequence after a prefix read.
4. GC: 399 of 400 unforced self-referencing gathers are reclaimed, the step-0
   baseline (asserted in the oracle).
5. Local tests changed to Rakudo's behaviour, each checked against `raku`:
   `finite-gather-pipe-reify.t` (`.grep` twice on one gather throws),
   `gather-lazy.t` (a Seq does not bind to `@`), `gather-seq-slip.t` (`.Slip`
   twice throws), `take-rw-shared-cell.t` (`my @g = gather ... take-rw` copies
   the values), `gather-infinite-coroutine.t` (`.head`/`.first` consume).
6. New `ValueView::LazyList` code for plain gathers exists only at the
   hand-off boundaries above (and the `touch_flat_operand` arm for a handed-off
   list); retiring them, and step 4, is #13026.
7. Creation plus drain (`tmp/bench-gather.raku`: 20,000 gathers of 20 takes
   each, read with `.elems`, plus one 200,000-element gather read with
   `.List`), release builds, median of 5 wall-clock runs on the remote
   container: 0.355 s at the merge base, 0.329 s with the adapter (ratio 0.93,
   within noise; no regression).

### 8.2 Builtin routine arguments (#13026 step 1)

`call_function_arms` and `try_native_function` no longer hand a gather argument
over as its shared inner list. `builtin_gather_args` (`src/vm/vm_gather_seq.rs`)
reads it the way Rakudo's routine does, measured per routine against `raku`:

- **Stream** (`first`, `zip`, `roundrobin`, `flat`, `map`, `grep`): the routine
  streams its input, so an infinite gather works. The coroutine is stolen and
  the Seq consumed, the §2.3 steal; a cached gather is shared, as for the
  methods.
- **Consume** (`sum`, `min`, `max`, `minmax`, `unique`, `repeated`, `squish`,
  `pick`, `roll`, `classify`, `categorize`, `produce`, `reduce`, `keys`,
  `values`, `kv`, `pairs`, `permutations`, `combinations`, `cross`, `append`,
  `prepend`): pulled whole through `take_seq_body`, which consumes an uncached
  Seq.
- **Cache** (every other routine: `elems`, `join`, `sort`, `reverse`, `set`,
  the `Str`/`Numeric` coercions): pulled whole through `reify_seq_body`; the Seq
  stays readable.
- **Pass** (`list`, `item`, `defined`, `push`, `unshift`): the Seq itself.

A Seq an earlier read consumed raises `X::Seq::Consumed` at the same boundary
when the routine streams or iterates it (`sum(s); sum(s)`), whatever its
source; a Cache-class routine may still take it, since that class also holds
routines that never read their argument (`infix:<~~>` against a type, roast
`S32-list/seq.t`). `try_native_function` writes the read
back into the caller's argument vector, so a caller that falls through to
another dispatch path after `None` passes on the read argument rather than the
now-consumed Seq. Before this, the pure builtins read the shared list's empty
cache: `sum(gather {...})` answered 0, `max` the whole list, `set` an empty Set,
`uc` and `chrs` the empty string. `t/collections/lazy-seq/gather-builtin-args.t`
pins the rows. The remaining hand-offs are the methods of built-in receivers,
`.AT-POS` / `.EXISTS-POS`, and `gather_operand` for zip/cross metaoperators and
reductions.

### 8.3 Built-in receivers' method arguments (#13026 step 2)

`call_method_with_values_unviewed` no longer hands a gather argument over as
its shared inner list, and the `CallMethod` / `CallMethodMut` opcodes, which
never did and read the gather's empty seed (`Set.new(gather {...})` was
`Set()`, #13024), now read it too. `method_gather_args` applies the §8.2
classes to a method of a built-in receiver, measured against `raku`:

- **Consume**: `new` (`Set.new`, `Bag.new`, `Array.new`), `append`,
  `prepend`.
- **Pass**: `push`, `unshift` (the Seq is stored as one element), and
  `Supply.from-list`, which pulls the gather itself: a gather that dies
  becomes the materialized supply's `quit_reason`, so a `whenever`'s `QUIT`
  phaser sees it (the supply-block `whenever` path now honours a static
  supply's `quit_reason` too, as the `react` path did since #12986).
- **Cache**: every other method (`split`, `splice`, `polymod`, `List.new`).

A user object's method, a role's method and a method the program declared on a
type object receive the Seq unchanged. `shared_gather_list` /
`shared_gather_args` are deleted; `600.polymod(gather { loop {...} })` hung
before this change too, and still does (a `Cache` read pulls the whole gather).
`t/collections/lazy-seq/gather-receiver-args.t` pins the rows. The remaining
hand-offs are `.AT-POS` / `.EXISTS-POS` and `gather_operand`.

### 8.4 `.AT-POS` / `.EXISTS-POS` (#13026 step 3)

`reify_or_consume_seq_target` no longer answers `.AT-POS` / `.EXISTS-POS` on a
gather with the shared inner list. `pull_pos_method_gather_prefix` pulls the
prefix the index needs into the Seq's body, through `reify_seq_body_prefix`
with the bound `$g[$i]` uses (`seq_subscript_needed_count`), and caches the
Seq. An index with no static bound pulls the whole gather. The method then
reads the body's stored elements, as for any other reified Seq. The call runs
from `take_seq_prefix` on the `CallMethod` / `CallMethodMut` paths and before
`call_method_with_values`' single authoritative Seq touch.
`t/collections/lazy-seq/gather-at-pos-prefix.t` pins the rows, including how
far an infinite gather's body runs. The remaining hand-off is `gather_operand`
for the zip/cross metaoperators and reductions. The streaming builtins of §8.2
and the §2.3 method steals also use it; those consume the Seq, as invariant 1
allows.

### 8.5 Reduction and zip/cross operands (#13026, first `gather_operand` slice)

A reduction no longer takes a gather operand as its inner list.
`reduction_operand` (`src/vm/vm_gather_seq.rs`) reads the operand through
`iterated_operand`, which is what Rakudo's iterating operators do with any
`Seq`, measured against `raku`:

- An uncached `Seq` (a gather, a `.map`, a `$`-itemized one under a
  reduction) is pulled whole through `take_seq_body` and consumed: `[+] s;
  s.elems` raises `X::Seq::Consumed`. Before this a reduction consumed only a
  gather, and only through the steal.
- A consumed `Seq` raises `X::Seq::Consumed` at once (`s.List; [+] s`, and
  `s.List; s Z 1`). Before this the reduction re-read it.
- `[max]` / `[min]` cache the Seq instead (`[max] s; s.elems` answers):
  Rakudo calls the operator's list candidate, whose `+args` binding caches it.
  `[\max]` consumes like every other triangle form.
- The triangle form, and the zip and cross metaoperators, still stream a
  gather through `gather_operand`, consuming the Seq: `([\+] gather { loop
  {...} })[^5]` and `(gather { loop {...} }) Z 1..3` must not pull the whole
  gather. The zip and cross metaoperators now consume a `.map` Seq too.
- `$s Z 1` and `$s X 1` read an itemized Seq as one element, as Rakudo does
  for an itemized Array; it is not consumed.

`t/collections/lazy-seq/gather-operator-operands.t` pins the rows. Rakudo
2026.09 dies on a triangle reduction over a *finite* gather (`[\+] gather {
take 1 }`: "continuationreset requires a continuation"), so no row pins that
shape.

`gather_operand` still has three callers that stream: the triangle form, the
zip and cross metaoperators, and the §8.2 Stream class. Retiring it needs a
`LazyList` pipe that pulls a `SeqBody` instead of the coroutine list. Pulling
a gather body one element at a time through `extend_gather_prefix` is
quadratic today: every pull publishes a whole new generation, and the
generation store keeps every superseded one (`seq_body/generations.rs`). So
the pipe needs an append-only generation first, or a chunked pull that does
not run the gather body ahead of what the consumer asked for.

### 8.6 The lazy Slip (#13117, amendment 2026-10-15)

A Rakudo `Slip` is a `List` that reifies on demand, so `.Slip` and `slip(...)`
of a lazy source must not pull it. mutsu's `Value::Slip` holds its items
eagerly. This amendment records how a lazy one is represented and which part
is decided but not yet built.

**Decided and implemented: the lazy-tail Slip.** A Slip of a genuinely lazy
list (`LazyList::renders_lazy_placeholder`: an infinite Range, a `.lazy`
gather, a lazy `.map`, a scan) is a `Value::Slip` holding that list whole as
one item. This is the shape `|EXPR` already gave a lazy operand, which the list
constructor turns into a lazy tail (`lazy_literal_with_slipped_tail`), so no
new `Value` variant is added and every existing Slip consumer keeps working:
an eager flatten site sees one lazy list element, which is where an eager
context over an infinite list stops in mutsu anyway.

- `.Slip` (`Interpreter::lazy_slip_coercion`, ahead of the dispatch paths that
  copy a receiver's elements, and the `Slip` coercion arm) and `slip(+args)`
  under the single-argument rule build it through
  `builtins::lazy_slip::lazy_slip_tail`.
- `Value::is_lazy_tail_slip` recognises it. A method call on it
  (`CallMethod`, `CallMethodMut`, `call_method_with_values`), a positional
  subscript and the `DeSlip` op of a list-operator argument read the lazy
  concatenation of its items (`Interpreter::lazy_slip_receiver`); a method
  about the Slip itself (`WHAT`, `WHICH`, `.Slip`, `.raku`, `.Str`, `.item`,
  `.defined`, the `^` metamethods) keeps the Slip. Its gist is the lazy list
  placeholder `(...)`.

**Decided, not yet built: a gather's Slip.** A plain `gather` is not
genuinely lazy (`.is-lazy` is `False` in Rakudo too), yet `slip(gather { loop
{ take ... } })` and `.Slip` on it must not pull the body: Rakudo's Slip
reifies from the gather's iterator. A gather therefore needs the same
lazy-tail shape with the stolen gather list (§2.3) as its tail. That cannot
land on its own: a *finite* gather's Slip must still flatten into an eager
list (`(1, 2).map({ slip(gather { take $_; take $_ }) })` is `(1 1 2 2)`), and
the 368 `ValueView::Slip` matches (179 files, measured 2026-10-15) flatten a
Slip's items without an interpreter to run the coroutine. The prerequisite is
to route those flatten sites through one interpreter-side chokepoint that
reifies a gather tail (the `|EXPR` forcing in `exec_make_slip_op` is the one
site that does it today), after which `.Slip`, `slip(...)` and `|EXPR` of an
unpulled gather can share the lazy-tail representation. Until then a gather's
Slip is pulled whole, as before, so an infinite gather's `.Slip` still hangs.

**Interim, built (2026-10-21, #13117).** The interpreter-side flatten sites
now go through one chokepoint, `Interpreter::reify_lazy_slip_tail`. Those
sites are a `.map` callback's result (`reify_finite_pipe_value`, the native
and multi-parameter map loops), `take` and a `flat` argument. The chokepoint
reifies each lazy item of a lazy-tail Slip that ends. A gather's item gets
the bounded pull `reify_finite_pipe_value` gives a gather. `.Slip` and
`slip(...)` of a plain gather (`Interpreter::gather_slip`) use the same
bounded pull, of `MAP_RESULT_GATHER_FORCE_CAP` elements:

- A gather that finishes within it gives the eager Slip of its elements.
  The pure flatten sites therefore never see the gather tail of a finite
  gather.
- A gather that does not finish gives a lazy-tail Slip. Its one item is a
  `lazy`-marked pipe that streams the gather, with the pulled prefix
  cached.

The bounded pull still runs a gather's body ahead of what is read. Rakudo
reads it on demand. The decided design above, with no pull at creation,
remains the target. A lazy-tail Slip returned by a `.map` callback whose item
does not end is still kept whole by the map's output: the output does not
stream it (#14295).
