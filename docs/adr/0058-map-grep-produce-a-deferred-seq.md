# ADR-0058: `.map`/`.grep` produce a deferred `Seq` — the callback runs at first consumption, not at the call

- **Status**: Accepted (step 2 shipped 2026-09-07; steps 3-4 open)
- **Date**: 2026-08-22
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
  (the `SeqBody`/`SeqSource` machinery this ADR extends, and whose §6 scoped this
  defect out of itself), [ADR-0038](0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md)
  (the other open `SeqBody` decision)
- **Ticket**: [todo/deep/residual-try-cell-eager-seq-reification-divergences.md](../../todo/deep/residual-try-cell-eager-seq-reification-divergences.md)

---

## 1. Context

### 1.1 The symptom family

A `map` callback that dies inside a `try` is caught by that `try` in mutsu and
escapes it in rakudo:

```raku
try { (1..3).map({die "boom"}) }; say "alive ", $!.defined
# raku:  dies, uncaught ("boom")
# mutsu: alive True
```

The whole "residual try-cell" family in the ticket — P4, P5, P12, P13, P18, Q9,
Q11, Q14 — is this one snippet in eight shapes (through a sub, through `EVAL`,
with `fail` instead of `die`, with the value used instead of sunk, with an
enclosing `CATCH`). mutsu is *more forgiving* than raku in every one: it never
aborts a file raku would pass, only passes constructs raku would abort.

All twelve cells the ticket lists were **re-measured on 2026-08-22 against a
current `main` build**, and all twelve still reproduce exactly as recorded —
none had been fixed by an intervening change:

| Cell | raku | mutsu |
| --- | --- | --- |
| P4 `try { (1..3).map({die "boom"}) }; say "alive ", $!.defined` | throws | `alive True` |
| P5 same via `sub f` | throws | `alive True` |
| P12 / P13 `sub ee { try { f() }; $! }` | throws | `X::AdHoc` + alive |
| P18 `sub ee { try { f() } }` (value used) | `Seq` + alive | `Nil` + alive |
| Q5 / Q6 yada-stub map under a sub-scope `try` | throws | `Failure` + alive |
| Q9 `try { … }; CATCH { default { … } }` | `unit-caught` | alive, nothing caught |
| Q11 `try { EVAL $c }` in a sub | throws | `X::AdHoc` + alive |
| Q14 `fail` instead of `die` | throws | `alive True` |
| R6 / R7 Q5 / Q6 with a tail marker | throws | `Failure` + alive |

### 1.2 The ticket's stated root cause is not the one these cells exercise

The ticket (and ADR-0034 §6, quoting it) says mutsu "forces a `map`-produced
`LazyList` at the assignment/call boundary, where raku keeps it lazy until
something actually consumes it", and points at `force_lazy_list_vm`'s callers.

That description does not fit the cells. `(1..3)` is a **finite** range, so
`Interpreter::is_lazy_pipe_source` (`src/runtime/methods_collection.rs`) is
`false` and no `LazyList` is ever built: `dispatch_map_method`
(`src/runtime/methods_dispatch_match2.rs`) materializes the source and calls
`eval_map_over_items` **immediately**, inside the `try`. There is no deferred
value whose force could be moved — the callback has already run by the time the
`try` block's tail value exists.

mutsu's `try`/sink *placement* is not at fault either, and must not be touched:
`compile_try_region` (`src/compiler/helpers_control_flow.rs`) deliberately
leaves the tail value on the stack and lets the **enclosing** statement's
`SinkPop` force it, outside the trap — which is exactly rakudo's rule, verified
in `news/2026-08/try-statement-sink-semantics-pinned.md` and pinned by
`t/try-sink-semantics.t`.

**The real root cause is that `.map`/`.grep` are eager in mutsu and lazy in
rakudo.** Everything else in the family follows from that.

### 1.3 mutsu has three regimes for one Raku operation

| Source / callback | Result | Callback runs |
| --- | --- | --- |
| infinite/lazy source, arity-1 callback (`is_lazy_pipe_source` + `make_lazy_pipe`) | `LazyList` with a `lazy_pipe` | on pull |
| any source, callback body contains `return` **or** is a `...` stub (`create_lazy_map_list`) | `LazyList` with `__mutsu_lazy_map_items`/`_func` | on force |
| **everything else — the overwhelming majority** | `Seq`, already reified | **at the `.map` call** |

The second regime is the tell: mutsu already *knows* the callback has to be
deferred whenever running it early is observable, and enumerates the two shapes
where that had bitten hard enough to fix (`return` needs
out-of-dynamic-scope detection; a `...` stub must not fire while the Seq is
never iterated). `die`/`fail` inside the callback is a third shape of the same
thing, and "the body contains a `die`" is not a predicate worth writing — the
`die` can be indirect, behind a call, behind an operator that fails. The
enumeration is a band-aid whose list can never be complete.

Non-`try` shapes make the divergence visible without any exception at all:

```raku
my $s = (1..3).map({ say "side $_"; $_ });
say "before";
say $s.List;
# raku:  before / side 1 / side 2 / side 3 / (1 2 3)
# mutsu: side 1 / side 2 / side 3 / before / (1 2 3)
```

`grep` behaves identically in rakudo (lazy) and identically in mutsu (eager over
a finite source), and does not even have the `return`/stub deferral that `map`
has.

### 1.4 Four of the twelve cells are a *different*, narrower bug

Q5, Q6, R6 and R7 use a `...` **stub** callback, which mutsu already defers
(regime 2 in §1.3), so eagerness is not their problem. Measured on the same
build, mutsu matches raku exactly for the stub map as soon as the enclosing
`try` is removed:

```raku
sub ee { map -> $x, $y { ... }, 1..6; say "reached-tail"; "done" }
say ee(); say "alive";       # both: "Stub code executed", exit 1
say ee().^name; say "alive"; # both: Failure / alive, exit 0
```

Add the `try` back (`sub ee { try { map … }; say "reached-tail"; $! }`) and raku
throws while mutsu answers `Failure` and runs on. So for these four the force
already lands in the right place, at the enclosing statement's `SinkPop` outside
the trap; what differs is **how a `fail` raised during that force resolves when
a `try` is lexically between it and the routine** — mutsu lets it return from
the routine as a `Failure`, rakudo throws it. That is out of scope for this ADR
and is pinned as two `todo` rows in
`t/map-callback-runs-at-consumption.t`; the other ten rows of that file are
already-correct behaviour this ADR must not regress.

### 1.5 Why this only became tractable now

ADR-0034 gave `Seq` a real body with a *deferred source* (`SeqSource`) and a
reify/consume split, and taught the dispatch chokepoints, `for`, sink and
`@`/`%` assignment to touch a not-yet-pulled body. Before that, deferring a map
meant every extra touch of the deferred value was a chance to hit the
destroy-on-materialize bug. That coupling is gone: `SeqSource` is now the
natural place for "these elements come from running a callback over a list", the
same way `SeqSource::IoLines` is the place for "these elements come from a
filehandle".

What is *not* gone is the read-path exposure, and that is what makes this an ADR
rather than a patch — see §5.

---

## 2. Decision

**`.map` and `.grep` return a `Seq` whose body is not yet reified. The callback
runs when something consumes the Seq, through ADR-0034's existing
`reify`/`take` split — not at the `.map` call. The `return`/stub deferral
predicate and its `LazyList` detour are retired.**

Concretely: `SeqSource` gains a `MapGrep { items, func, is_grep }` variant
(§3.4), `Interpreter::pull_seq_source` gains the arm that runs the callback, and
`dispatch_map_method` hands back `Value::seq_deferred(..)` instead of
`Value::seq(items)`. No new `Value` variant and no new forcing rules: every site
ADR-0034 already taught to reify-or-consume a `ValueView::Seq` that
`needs_touch()` covers this for free.

Two consequences are accepted up front, because they are the point:

- **mutsu becomes stricter.** A `die`/`fail` in a `map` callback under a `try`
  now escapes that `try`, matching rakudo (§1.1). Every whitelisted roast file
  that relies on mutsu's permissiveness must be fixed, not exempted.
- **Side-effect ordering changes.** A `map` callback's side effects happen at
  first consumption, not at the call. That is rakudo's observable behaviour
  (§1.3) and any local assertion that depends on the old order was pinning a
  mutsu artefact.

What is *not* decided here: whether the deferred body pulls one element at a
time or reifies the whole source on first touch. This ADR reifies whole (the
cheapest thing that is correct); §3.3 records the pull-granular refinement as a
follow-up.

---

## 3. Options considered

| Option | Matches rakudo? | Mechanism | Blast radius | Verdict |
| --- | --- | --- | --- | --- |
| **0.** Extend the deferral predicate with "body contains `die`/`fail`" | ✗ (indirect throws) | one predicate | tiny | Rejected — §3.1 |
| **1.** Route every `.map` through `create_lazy_map_list` (`LazyList`) | ~ | existing | wide, plus a per-map `Env` clone | Rejected — §3.2 |
| **2.** Make `is_lazy_pipe_source` true for finite sources too (`lazy_pipe`) | ~ | existing | wide; single-element pull only | Rejected — §3.3 |
| **3.** A new `SeqSource::MapGrep`, reified by ADR-0034's `reify`/`take` | ✓ | ADR-0034's | wide, but on the *already-supported* deferred-Seq shape | **Recommended** |

### 3.1 Why not widen the deferral predicate

`dispatch_map_method` already defers when
`body_contains_return(..) || is_stub_routine_body(..)`. Adding a third
syntactic probe for `die`/`fail` fixes the literal snippets in the ticket and
nothing else: `(1..3).map({ f() })` where `f` dies, `(1..3).map({ 1/0 })`,
`(1..3).map({ @a[10].method })` all still diverge. A predicate that has to
enumerate "ways a callback can throw" is unfinishable by construction — the same
shape ADR-0034 §1.4 rejected for consumption method lists. It is also exactly
the band-aid CLAUDE.md's gain/risk definition calls a *risk* (an ad-hoc
mechanism whose failure mode is silent divergence).

### 3.2 Why not route every map through the existing `LazyList` deferral

`create_lazy_map_list` (`src/runtime/methods_dispatch_match2.rs`) already does
the right thing semantically — it stores the source items plus the callback and
runs `eval_map_over_items` at force time, and `resolution_lazy.rs` documents
that this keeps "full fidelity with eager map: block arity > 1, Slip flattening,
LAST/NEXT phasers, and composed callbacks". Flipping both call sites
(`dispatch_map_method` and `builtin_map`) to always take it is a two-line change.

It is still the wrong mechanism:

- **It clones the whole `Env` per `map` call.** `create_lazy_map_list` does
  `let mut env = self.env.clone()` and stuffs the items and the callback into
  it, because `force_lazy_list` installs `list.env` as the interpreter env. On
  the hottest list operation in the language that is a per-call allocation of
  the entire lexical scope, paid whether or not the Seq is ever forced.
- **It picks the wrong Raku type by accident.** The value is a `LazyList`, and
  `value_type_name` only answers `Seq` for it because `create_lazy_map_list`
  sets the `__mutsu_lazylist_from_gather` marker — a `gather` flag standing in
  for "this is a Seq". mutsu would then have `map` produce a *fourth*
  representation of a lazy sequence at exactly the moment ADR-0034 finished
  collapsing three into two.
- **It moves work away from the machinery that was just built for it.** ADR-0034
  taught every dispatch chokepoint, `for`, `SinkPop`, `ExecCall`'s sink and
  `@`/`%` assignment to reify/consume a `ValueView::Seq` whose body
  `needs_touch()`. A `LazyList` needs a parallel set of forcing rules, which is
  what the 37 `force_lazy_list_vm` call sites already are.

Its one virtue is that it is *cheap to try*, which makes it the right vehicle
for **measuring** the read-path exposure (§5) before committing to option 3 —
see §4 step 0.

### 3.3 Why not make the existing `lazy_pipe` cover finite sources

`is_lazy_pipe_source` returning `true` for any source would reuse
`force_lazy_pipe`'s genuine one-at-a-time pull, which is strictly closer to
rakudo than a whole-list reify. But `make_lazy_pipe` bails out for any callback
with arity > 1 or a slurpy parameter (single-element pull cannot reproduce
chunked binding), so `map -> $x, $y { }, 1..6` — half the ticket's cells — would
stay eager, and the two regimes would have to coexist anyway. It also inherits
`LazyList`'s type-name problem from §3.2. Worth revisiting *after* option 3 as a
pull-granularity refinement, not as the fix.

### 3.4 The recommendation

Give `SeqSource` a variant for "these elements come from running a callback over
a list", exactly parallel to `SeqSource::IoLines`:

```rust
pub(crate) enum SeqSource {
    Reified,
    Iterator(Value),
    IoLines { handle: Value, words: bool, kv: bool },
    /// `.map`/`.grep` over an already-materialized source: run `func` over
    /// `items` on first touch. rakudo's `Seq` from `map`/`grep`.
    MapGrep { items: Arc<Vec<Value>>, func: Value, is_grep: bool },
    Taken,
}
```

`Interpreter::pull_seq_source` (`src/vm/vm_helpers_lazy.rs`) gains one arm that
calls the existing `eval_map_over_items` / grep loop, and `dispatch_map_method`
returns `Value::seq_deferred(SeqSource::MapGrep { .. })` instead of
`Value::seq(items)`. Everything else — reify-in-place, idempotence, `.cache`,
`X::Seq::Consumed`, `Trace`, the consumption matrix — is ADR-0034's, unchanged.
No new `Value` variant, no new type-name special case, no `Env` clone: the
callback `Value` already carries its own closure environment.

---

## 4. Migration plan

| # | Step | Notes |
| --- | --- | --- |
| **0** | **Measure the read-path exposure** by flipping `dispatch_map_method`/`builtin_map` to always call `create_lazy_map_list` behind a temporary env gate, and running `t/` and the roast whitelist with it on. This is option 3's exposure without option 3's cost, and it produces the list of consumers that read a deferred sequence without forcing it. | Discard the gate afterwards; it is a measurement, not a slice. |
| **1** | **Done (2026-08-22).** `t/map-callback-runs-at-consumption.t` — 23 rows, raku-verified 23/23, mutsu 12 passing and 11 `todo`. Un-`todo`ing the nine ADR-0058 rows is this ADR's completion signal; the other two `todo`s belong to §1.4's separate bug. | Same shape as ADR-0034 phase 1. |
| **2** | **Done for SOME receivers only (2026-09-07); see the caveat below.** `SeqSource::MapGrep` + the `pull_seq_source` arm + `Value::seq_deferred` construction in `dispatch_map_method` only (not `builtin_map`, not `grep`), plus the read-path consumers S8 lists. | All nine ADR-0058 rows of phase 1's oracle are un-`todo`d; the two remaining `todo`s are S1.4's separate bug. |
| **3a** | **Done (2026-09-07).** `builtin_map` (the `map &f, @xs` listop form) returns the same `Value::seq_deferred(SeqSource::MapGrep { .. })` as step 2. Attempted and reverted earlier the same day behind a step-2 hole (S9.1); that hole is closed (`news/2026-09/deferred-map-callback-frame.md`). The mandatory full `make roast` then found three more consumers, all fixed generally -- see S9.3. | `t/listop-map-defers.t`, and `t/nested-deferred-map-seq-is-pulled.t`'s listop row un-`todo`d. |
| **3b** | Extend to both `grep` entry points. Measured 2026-09-07: grep is still fully eager and diverges from rakudo. `grep`'s `:k`/`:kv`/`:p` adverbs need positional indices over the whole result and can stay eager, exactly as they already opt out of `make_lazy_pipe`. **Its own slice, for a measured reason -- see S9.2.** | |
| **4** | Retire the `body_contains_return` / `is_stub_routine_body` deferral predicate and `create_lazy_map_list` — both become dead once every map defers. | The maintainability payout. |

### Verification

```sh
prove -e target/debug/mutsu t/map-callback-runs-at-consumption.t   # the oracle
prove -e target/debug/mutsu t/try-sink-semantics.t t/seq-*.t t/lazy-*.t
raku t/map-callback-runs-at-consumption.t                          # 23/23, the reference
make roast      # mandatory: this makes mutsu STRICTER in every ticket cell
```

---

## 5. Risks

- **This makes mutsu stricter, deliberately, in every cell of the ticket.**
  Constructs that pass today start aborting, matching raku. A full `make roast`
  is mandatory, not `make test` alone, and the PR should expect a fix-forward
  round. (CLAUDE.md: a temporary CI failure is the safety net working, not a
  cost.)
- **The read path is the real exposure.** There are 356 `ValueView::Seq(` match
  sites, 292 `value_to_list(` calls and 16 `flat_val(` calls in `src/`. All of
  them read a `SeqBody` through `Deref`, which by design returns the *empty
  seed* for a body nobody has reified — ADR-0034 §2.1 chose that deliberately so
  a read can never re-enter the VM. Today only `IO::Handle.lines` and
  `Seq.new($iterator)` produce such bodies, so the exposure is rare; making
  every `.map` deferred exposes all of them at once. ADR-0034's own outcome
  (§7.1) already found two such consumers (`Value::eqv`'s `(Seq, Seq)` arm and
  the `...` sequence generator) that its §1.5 site inventory had missed. Step 0
  exists to turn this from a guess into a list.
- **Side-effect *ordering* changes program output, not just exception timing.**
  Any `t/` or roast assertion that depends on a `map` callback's `say`/push
  happening before a later statement will flip. Most such assertions are testing
  the wrong thing (mutsu's eagerness), but each one has to be re-checked against
  `raku` rather than "fixed".
- **Perf: neutral in principle, unmeasured in practice.** A deferred map trades
  the callback loop for one `Arc<SeqBody>` allocation at the call and the same
  loop at first touch; a map whose result is *never* consumed becomes free. But
  every consumer now pays a `needs_touch()` state check plus, on first touch, a
  `Mutex` acquisition. Do not sell this as an optimization without a bench row;
  do watch for a regression on the map-heavy benches.
- **`grep`'s adverbed forms and the rw path stay eager**
  (`eval_map_over_items_rw` writes back into the source array, which is
  meaningless to defer). Two regimes remain after step 4, but they are split on
  a real semantic line (does the operation write back?) rather than on a
  syntactic probe of the callback body.

---

## 6. Scope: what this ADR does not decide

- **The `LazyList` pipe (`lazy_pipe`) over genuinely infinite sources stays as
  it is.** Making it and `SeqSource::MapGrep` one mechanism (a pull-granular
  deferred Seq) is the natural follow-up, and §3.3 records why it is not the
  first step.
- **`try`/sink placement is settled and out of scope** — see §1.2 and
  `news/2026-08/try-statement-sink-semantics-pinned.md`.
- **ADR-0038's `Seq`/`List` view question** is orthogonal; a `MapGrep` body
  presents as `Seq` like every other `SeqSource`.

---

## 7. Consequences

- **`map`/`grep` get rakudo's timing.** The whole try-cell family in the ticket
  aligns, and so does the plain, exception-free side-effect ordering divergence
  of §1.3 that nobody had written down before.
- **A third representation of a lazy sequence is avoided.** ADR-0034 collapsed
  three into two; routing map through `LazyList` (§3.2) would have made it three
  again. Routing it through `SeqSource` keeps the count at two and gives the new
  variant the reify/consume semantics for free.
- **Two syntactic band-aids die** — `body_contains_return` and
  `is_stub_routine_body` as *map-deferral* predicates, plus
  `create_lazy_map_list` and the `__mutsu_lazy_map_items`/`_func` env keys.
- **The read path becomes the load-bearing invariant.** After this, "a
  `ValueView::Seq` may be a body nobody has pulled yet" stops being a rare
  `IO::Handle.lines` corner and becomes the common case. That is a real,
  permanent maintenance obligation on every new `ValueView::Seq(items)` reader,
  and step 0 of §4 exists so it is entered with a list rather than a hope.
- **If rejected**: the ticket's cells stay divergent, the deferral predicate
  keeps growing one observable-eagerness bug at a time, and `map` keeps being
  the one core operation whose laziness mutsu decides by grepping the callback's
  AST.

---

## 8. Outcome of step 2 (2026-09-07)

### 8.1 What step 0 actually measured

The §4 step-0 probe was run as specified — `dispatch_map_method`/`builtin_map`
flipped to always call `create_lazy_map_list` behind a throwaway
`MUTSU_ADR0058_STEP0` env gate — against a current `main` build:

| Suite | Files failing under the gate |
| --- | --- |
| `prove -j4 t/` (3735 files) | **59** |
| roast whitelist, release binary (1436 files) | **55** |

**The proxy over-reported, and its excess was systematic, not random.** Roughly
half the `t/` hits were the `concurrent-*`/`thread-*`/`cas-*`/`shared-*` family,
which fail under option 1 for a reason option 3 does not have: §3.2's
per-`map` `let mut env = self.env.clone()` **snapshots** the captured lexicals,
so a shared cell mutated by a `start` block inside the callback is lost. The
real `SeqSource::MapGrep` implementation reuses the callback `Value`'s own
closure environment and none of those files ever failed. Under the real
mechanism the first full `t/` run showed **44** failures, converging to 0.

So step 0's value was **not** the count: it was confirming that the exposure is
a *bounded, enumerable list of element readers* rather than an open-ended one,
and that the failures cluster into a handful of funnels. Recommendation for a
future slice: run the exposure probe with the mechanism you intend to ship
(behind an env gate), not with a proxy — the proxy's own defects dominate the
signal, and the extra cost is zero because the mechanism has to be written
anyway.

### 8.2 The read-path consumers step 2 had to fix

`Interpreter::reify_map_grep_seq` / `_args` / `sink_map_grep_seq`
(`vm/vm_helpers_lazy.rs`) are the guard; they are tag-probed (`is_seq_value()`
before `view()`, because `view()` on a lazy `Match` materializes it — ADR-0016
P5) and no-op for every other `SeqSource`, whose streaming semantics must **not**
be forced at an argument boundary. Call sites, by funnel:

- **Argument boundaries** (a native/builtin reads elements as plain Rust
  values): `Interpreter::call_function` (gated on `is_builtin_function`),
  `try_native_function`, `try_native_method` (arguments only — the receiver is
  ADR-0034's job), `exec_call_method_mut` args, `call_compiled_closure`
  (a slurpy/`@_` parameter *flattens* a Seq), `call_sub_value`, `builtin_await`.
- **Rendering / coercion**: `exec_say_op`/`note`/`put`/`print`,
  `exec_str_coerce_op`, `exec_num_coerce_op`, `coerce_stringy_operand`.
- **Operators**: `eval_binary_with_junctions` (the shared binary funnel),
  `exec_meta_op` + `zip_iter_from_value` (`Z`/`X`), `exec_reduction_op`
  (`[+] @xs.map(...)`), `set_contains` (`(elem)`), `vm_smart_match`.
- **Assignment**: `SetGlobal` and `AssignExpr`/`AssignExprLocal` for an `@`/`%`
  target (the `my @a = SEQ` reify already existed only on `SetLocal`), and
  `IndexAssignExprNamed` **for a slice index only** — a single-element store
  itemizes the Seq into that element's Scalar container and stays unforced
  (measured: `my %h; %h<f> = (1..3).map({die}); say "alive"` prints "alive" in
  raku, while `@n[0,1] = (1,2).map({...})` is eager,
  `roast/S32-list/seq.t` #18). For the same reason `SinkPopAssign` no longer
  sinks a `MapGrep` body at all.
- **Composition / hyper**: the `__mutsu_compose_left/right` relay, the
  `hyper_race_wrap` re-wrap for a >=1000-element `HyperSeq.map`, and the
  per-element result push in `exec_hyper_method_call_op`.
- **`Test` handlers that discard a block's value**: rakudo's `dies-ok`,
  `lives-ok` and `throws-like` write `$code();` as a *statement*, so the block's
  Seq is **sunk** and the callback's exception escapes. mutsu calls the block
  natively and dropped the value, so those three now call `sink_map_grep_seq`
  explicitly.
- **The program's own tail statement** (`runtime/run.rs`), next to the existing
  `LazyList` drain — `(1,2,3).map({ die "oh noes" })` as a whole program must
  die (`roast/integration/weird-errors.t` #18).
- **`flatmap`**, which is implemented as `.map(...)` plus a pure flatten.
- **`ApplyVarTrait`**: `my %r is SetHash = %h.map: {...}` binds the Seq into the
  slot *before* the trait runs (the declaration emits `MarkBindContext;
  SetLocal`, which skips `exec_set_local_op_inner`'s `@`/`%` reify), and the
  QuantHash coercion reads the elements purely.

### 8.3 Premises that did not survive contact with the code

- **§4's step-0 recipe is a proxy, and a lossy one** — see §8.1.
- **`SeqBody::take` does not store what it pulls.** §3.4 assumed the existing
  reify/consume machinery would cover the new source "for free". It does for
  `reify`, but `take` on a genuinely deferred source hands the elements to the
  caller and leaves `gens` empty — and two consumers (`.iterator`'s arm in
  `reify_or_consume_seq_target_inner`, and `exec_hyper_method_call_op`) call
  `take` and then read the *same body* back through `Deref`. That was a latent
  ADR-0034 bug, invisible while only `IO::Handle.lines` produced deferred
  bodies; `SeqBody::store_taken_elements` fixes it without reviving the source.
  It is what made `method iterator() { self.pairs.iterator }` in a
  `does Iterable` role iterate nothing.
- **`use fatal` is lexical, so it cannot be read at pull time.**
  `eval_map_over_items` consults `self.fatal_mode`; deferring the loop moved
  that read from the `.map` call site to the *consumer's* dynamic context, which
  retroactively made a soft Failure fatal (`t/try-fatal-does-not-retroactively-flag-closure-seq.t`)
  and stopped a genuinely fatal one from throwing
  (`t/whatever-code-fixes.t`). `SeqSource::MapGrep` therefore carries a `fatal`
  field captured at the `.map` call and restored around the pull.
- **A deferred body needs the same caller writeback a `LazyList` force does.**
  The pull is the effective call site for the callbacks it runs, so it must call
  `reconcile_caller_after_lazy_force` exactly as `force_lazy_list_vm` does, or a
  captured-outer lexical the callback mutated (`LAST $ran = True`, `$count++`)
  never reaches the consuming frame's local slot.
- **`has_deferred_source()` must NOT include `MapGrep`.** Every call site of that
  predicate reads it as "single-use, steal it" — most visibly
  `reify_or_consume_seq_target_inner`'s `"list"` arm, whose entire compromise is
  that a `.map`/`.grep` result stays re-readable through `@$s`
  (`t/seq-array-context-reiterate.t`). `MapGrep` is deferred in *when the
  callback runs*, not in whether the result is re-readable.
- **A Seq read out of an `@`/`%` element arrives wrapped.** ADR-0040 itemizes it
  into a `Value::Scalar` box, so `reify_or_consume_seq_target_inner`'s
  `ValueView::Seq` match missed it entirely and `%h<k>.elems` answered 0. That
  function now looks through a `Scalar`/`ContainerRef` wrapper.

### 8.4 Known remaining holes (deliberately not closed here)

The infallible set operators (`exec_set_diff_op`, `exec_set_sym_diff_op`,
`exec_set_subset_op`/`superset`/`strict_*`) read their operands through pure
code and cannot propagate a callback's exception without a signature change;
they still read an unpulled `MapGrep` body's empty seed. `set_contains`
(`(elem)`/`(cont)`) is fixed but has to swallow a throwing callback for the same
reason. No test in `t/` or the roast whitelist exercises the remaining ones;
step 3 (which makes `grep` deferred too) should revisit them.

## 9. Step 3 attempt (2026-09-07): what it found, and why it is parked

### 9.1 The listop diff is trivial; the blocker is a step-2 hole

`builtin_map`'s non-rw tail called `eval_map_over_items` directly, so the
`map &f, @xs` form ran its callback at the call and answered a **`List`**:

```raku
my $s = map { $_ * 2 }, 1..3;
say $s.^name;        # rakudo: Seq       mutsu: List
```

Returning the deferred Seq there instead fixed the type, the side-effect timing
(`sub ee { my $s = map -> $x { say "RAN"; $x }, 1..3; say "T"; $s }` printed
`RAN RAN RAN T` and now printed rakudo's `T RAN RAN RAN`), `for map { ... }, 1..3`,
assignment to an `@` variable, `.elems` and the empty-source case, with the
`source_var` rw-writeback branch left eager. `make test` was green (3757 files /
39156 tests).

The **full `make roast`** this ADR's §5 makes mandatory then aborted
`roast/integration/99problems-21-to-30.t` with a stack overflow. Reduced:

```raku
sub g(@sizes) {
    return "STOP" if @sizes == 0;
    [1].map(-> $e { g(@sizes[1..*]).map(-> $x { $x }) })
}
say g((2,1)).raku;
```

rakudo terminates at depth 3 with `@sizes` empty; mutsu recurses forever
reading `@sizes` as depth 1's `(2, 1)`. **This reproduces on `main` with the
METHOD form**, so it is not step 3's bug -- step 3 only routes more programs
onto it. A deferred `MapGrep` carries `items`, `func` and `fatal` but **no
frame**, so `eval_map_over_items` runs the callback under whatever env is
active at the pull; the pre-ADR-0058 `create_lazy_map_list` snapshots
`self.env` at the `.map` call and gets the same program right.

Step 3 was therefore parked behind that hole.

**UNPARKED 2026-09-07** (`news/2026-09/deferred-map-callback-frame.md`). The
hole is closed, and none of the three options recorded for it was the answer --
all three revolved around *adding* a frame snapshot, and the callback's closure
env already carried the right `@sizes`. It was being discarded:
`eval_map_over_items` merges the captured env into the running frame with
**caller priority**, excepting only `self` and a captured `ContainerRef` cell.
The cell exception covers the lexicals `box_captured_lexicals` boxes -- in
practice `$`-scalars -- so an `@`/`%` container free variable, captured by
value, lost to the consuming frame's same-named lexical. The merge now lets a
captured value for one of the block's own `free_var_syms` win, which is the same
lexical-resolution rule the cell encodes, and costs a set lookup per captured
key rather than an `Env` clone. So step 3 and step 4 are unblocked, and §5's
mandatory full `make roast` is what still gates them.

### 9.4 Step 2 does not cover `@a.map` (measured 2026-09-07)

Step 2 put the deferral in `dispatch_map_method`, and a `.map` on a **named
array variable** never gets there: it compiles to `OpCode::CallMethodMut`, whose
VM-native array-method dispatch runs the map eagerly. Measured at `e7662519d`:
`@a.map` and `(@a).map` run their callback at the call, while `@a.List.map` and
`(1,2,3).map` defer. A `rust-gdb` breakpoint on this ADR's own deferred tail
never fires for `@a.map({ … })`, and one on `eval_map_over_items` does not
either -- so the eager loop is a THIRD implementation, not either of the two
this ADR knows about. `t/map-callback-runs-at-consumption.t`'s 23 rows do not
exercise that spelling.

This also explains the residual `try` rows: `try` implies `use fatal`, and
`SeqSource::MapGrep` already captures `fatal` at the `.map` call -- but the
`@a` spelling never builds a `MapGrep`, and the Range spelling goes through
`make_lazy_pipe`'s `LazyList`, which has no `fatal` field. The fix is not a rule
about `try`; it is routing every `.map` through one deferral that carries
`fatal`, i.e. step 4. Tracked in
`todo/deep/array-map-on-a-real-array-is-still-eager.md`.

### 9.3 What the mandatory roast run found when step 3 landed (2026-09-07)

`make test` was green (3779 files / 39681 tests) with step 3 in place, and the
full `make roast` still failed three files. All three were **general** defects
that eager `map` had been hiding, not step-3 special cases:

| file | defect | fix |
|---|---|---|
| `integration/99problems-21-to-30.t` | The capture merge in `eval_map_over_items` now *overwrites* a same-named key, but `touched_keys` -- the save/restore set around the map loop -- still listed only the keys the merge *introduced*. A nested deferred map therefore left its own capture behind for the enclosing map's NEXT iteration, and a recursive producer read the inner call's `@sizes` from iteration 2 on. | Every key the merge overwrites is saved and restored. |
| `S32-list/categorize.t` | `categorize`'s mapper result is read through `as_items`, a pure-value reader that cannot pull, so a mapper whose body is a `map` categorized nothing. The `LazyList` force beside it was the same guard for the older deferral. | `reify_map_grep_seq` on the mapper result. |
| `S06-other/main.t` | rakudo's `RUN-MAIN` **sinks** `MAIN`'s return value; mutsu dropped it. Invisible while `map` was eager. | `sink_map_grep_seq` on `MAIN`'s result (both the fall-off-the-end and the explicit-`return` paths). |

This is the §5 risk paying for itself twice: the run before step 3 caught the
frame hole, and the run with step 3 caught three consumers. Keep the full local
`make roast` for step 3b and step 4.

### 9.2 grep is eager too, and is its own slice regardless

Measured on the same build:

| probe | rakudo | mutsu |
|---|---|---|
| `my $s = (1..3).grep({ say "RAN"; $_ > 1 }); say "T"` | `T RAN RAN RAN` | `RAN RAN RAN T` |
| the listop spelling of the same | `T RAN RAN RAN` | `RAN RAN RAN T` |
| `sub ee { try { (1..3).grep({ die "boom" }) }; say "reached-tail"; 42 }` | throws, tail unreached | `reached-tail`, `42`, alive |
| `my @a=1,2,3; grep({$_=5}, @a).eager; say @a` | `[5 5 5]` | `[1 2 3]` |

Beyond the shared blocker, grep needs something `map` has no equivalent of:
`dispatch_grep`'s `ValueView::Array` arm **promotes each matched source slot to
a shared `ContainerRef` cell**, writes the promoted array back with
`overwrite_array_bindings_by_identity`, and builds the result out of the same
cells, so `for @a.grep(...) { $_++ }` mutates through into `@a`. Deferring grep
moves that promotion and its identity-keyed writeback to pull time, in a
different frame. That is also why
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` §B finds
`@a.list.grep({$_=5})` writing through while `@a.list.map({$_=5})` does not --
3b and that ticket's producer 1 are one decision.

`SeqSource::MapGrep`'s pull arm also runs `eval_map_over_items` unconditionally,
so 3b needs a grep mode on the variant (or a sibling variant) before any of the
above.

**Prerequisite landed 2026-09-07** (`news/2026-09/grep-promotion-is-published-in-place.md`).
The concrete blocker was sharper than "the promotion moves to pull time": the
promotion was published by building a REPLACEMENT `ArrayData` and re-binding it
with `overwrite_array_bindings_by_identity`, which walks the **current frame's
`env`**. A deferred grep promotes in a frame where the source's names are gone,
so that route cannot work at pull time at all -- and it was already silently
dropping the promotion for any source not lexically visible right there.
Publishing the promotion by mutating the source `Gc<ArrayData>` in place
(ADR-0013 §7 made this sound at the primitive) is frame-independent, reaches
every alias by construction, and drops the `pending_rw_writeback_sources` drain
the re-binding needed. Making the publication universal cost two follow-on
fixes, both for pre-existing bugs the old frame-dependent route had merely
hidden: two decont leaks in `Backtrace`'s frame readers, and -- caught by the
bundled-library battery gate on `URI mutate.rakutest` -- a closure's frame-exit
"rejoin an rw-argument writeback to the captured cell" step that fired for any
name on the *process-wide* retain-on-miss pending list, so a stale `_` entry let
one closure store the calling frame's ambient topic through the element cell a
`.map`/`.grep` block had captured. That is now gated on
`cc.capture_free_var_set()`. With both, `make test` and a full `make roast` are
unchanged. 3b's remaining work is the grep mode on the variant plus deferring
the two entry points.

### 9.3 What DID land from the attempt

Pulling a deferred `MapGrep` left the deferred `MapGrep`s it *produced*
unpulled, so the pure readers §8 exists for saw ADR-0034's empty seed one level
down: `[1].map({ [2].map({ "STOP" }) })` rendered `(().Seq,).Seq` for `.raku`,
`(())` for `.gist`, nothing for `.Str`, and nothing for `.flat.join`, where
rakudo gives the nested values. The pull arm now pulls the `MapGrep`s its own
pull produced, recursively. Pinned by `t/nested-deferred-map-seq-is-pulled.t`
(8 rows, raku-verified);
`news/2026-09/nested-deferred-map-seq-is-pulled.md`.
