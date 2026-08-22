# ADR-0058: `.map`/`.grep` produce a deferred `Seq` — the callback runs at first consumption, not at the call

- **Status**: Proposed
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
| **2** | Add `SeqSource::MapGrep` + the `pull_seq_source` arm + `Value::seq_deferred` construction in `dispatch_map_method` only (not `builtin_map`, not `grep`). Fix the consumers step 0 found. | `map` method form alone is enough to un-`todo` most of phase 1. |
| **3** | Extend to `builtin_map` (the `map &f, @xs` function form) and to both `grep` entry points. `grep`'s `:k`/`:kv`/`:p` adverbs need positional indices over the whole result and can stay eager, exactly as they already opt out of `make_lazy_pipe`. | |
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
