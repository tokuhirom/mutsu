# ADR-0096 — A battery is the real upstream module; the interpreter is what grows

- Status: Accepted
- Date: 2026-09-12
- Issue: [#8184](https://github.com/tokuhirom/mutsu/issues/8184)
- Standing policy this records: [BATTERIES.md §1](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is), and the ban clause in `CLAUDE.md`
- Related: [#8183](https://github.com/tokuhirom/mutsu/issues/8183) (the JSON interception), [#7560](https://github.com/tokuhirom/mutsu/issues/7560) (NativeCall)

## Context

mutsu's product claim is compatibility: *real ecosystem code runs*. Every
downstream artifact rests on one rule — **vendor the upstream module unchanged
and grow the interpreter until it runs (BATTERIES.md rung 2); do not reimplement
the module natively (rung 3)**. It decides what a battery *is*, it is the reason
ADR-0085's parity KPI means anything, and it is the reason a whole class of
"just ship a shim" shortcuts is unavailable.

That rule was a user decision (2026-08-01) and has been recorded only as prose,
in `BATTERIES.md` §1 and in `CLAUDE.md`'s working agreements. Prose is where the
rule lives; it is not where the *reasoning* lives. Three things in particular
have no home:

1. **The rejected alternative.** Nothing states why native reimplementation
   loses, so each new case re-argues it from scratch — usually on effort, which
   is the axis the policy deliberately does not decide on.
2. **The exceptions.** Two survived into this review (`NativeCall`, and the JSON
   `to-json`/`from-json` fast path). One is justified in writing and one was
   declared "permanent policy" inside a battery's own selection record — a
   document that has no standing to make a policy decision, and which
   [#8183](https://github.com/tokuhirom/mutsu/issues/8183) showed was arguing
   from an expired premise before retiring it (E2).
3. **The clause the JSON carve-out was allowed to skip**: that a performance
   measurement justifies an *optimization*, never a *substitution*. Leaving it
   implicit is what let a 1000x benchmark buy an exemption the policy does not
   offer.

There is also a citation that has outlived its accuracy.
`news/2026-07/nqp-op-layer-measured-and-rejected.md` is quoted as "do not build
an `nqp::` op layer", and mutsu has since built one — **111 ops across ~1,790
lines** (`src/runtime/nqp_ops{,_builtin,_process,_str,_text}.rs`,
`src/vm/vm_call_nqp.rs`), still growing. The blanket form keeps being cited
against work that is already happening, and no document says otherwise.

This ADR records the decision, its rejected alternative, its exception
discipline, and the accurate form of that measurement. It does not itself change
any code.

## Decision

### D1. A battery is the upstream module, vendored unchanged

The ordering in BATTERIES.md §1 is normative and strict: adopt verbatim (rung 1)
→ grow the interpreter when it does not run (rung 2) → self-implement only as a
genuine last resort (rung 3) → push improvements upstream once mutsu carries
weight (rung 4). A gap found while running a battery is fixed **in the
interpreter**, as a general improvement. The vendored tree is never hand-edited,
and the module is never rewritten to route around the gap.

### D2. Native provision (rung 3) is banned going forward

No new module may be provided by a mutsu-internal reimplementation. "The
implementation is large" is not a reason to fall back to rung 3 — a large rung-2
bill is the *expected* shape of this policy, not a signal to abandon it. If a
case appears to make rung 3 genuinely unavoidable, it stops and goes to the
user; it is never shipped unilaterally.

**Why the alternative loses.** A module that only *looks like* the upstream one
is a private dialect (BATTERIES.md §1). Concretely, that costs:

- **The compatibility signal, which is the point.** Running the genuine module
  is the evidence; running a lookalike measures mutsu against mutsu. ADR-0085's
  parity KPI is only meaningful because both sides execute the same source —
  exactly the argument that made retiring the native `Test` provider a
  precondition for that campaign.
- **Silent divergence, forever.** Two implementations under one name drift on
  every edge the tests do not cover, and the divergence surfaces as a user's bug
  report against the upstream module, not against mutsu.
- **A maintenance tail with no upstream.** Every upstream release is a diff
  someone has to re-implement, and no upstream fix — security fixes included —
  reaches the user through the normal channel.
- **The resolution ladder.** BATTERIES.md §6 makes the bundle the *floor*, so a
  newer or patched module installed with `mzef` shadows it. A native provider
  keyed on a module name jumps that ladder, and the override path a security
  update depends on stops existing.

### D3. A performance measurement justifies an optimization, never a substitution

This is the clause the earlier prose left implicit. The distinction is not about
how large the measured gap is; it is about what the program can observe.

- An **optimization** is selected transparently and is semantically
  indistinguishable from the code it replaces. The JIT is the reference example:
  it changes how bytecode executes and nothing about what the bytecode means. A
  specialization of a vendored module's own code path qualifies if — and only if
  — no program can tell which path ran.
- A **substitution** changes what the program observes: a different exception
  type, a different error message, a different edge-case answer, or a different
  module resolving under the same name. It is banned, and **no benchmark buys an
  exemption**.

The private-dialect argument in D2 does not weaken when the divergence is bought
with speed instead of with convenience. A measured gap is a reason to make the
real thing fast; it is a reason to schedule work, not to keep an unexamined
mechanism.

Two live examples, one on each side of the line:

- **Legitimate.** The parse-time `TEST_EXPORTS` list
  (`src/parser/stmt/simple/module_exports.rs`) short-circuits scanning
  `Test.rakumod` for its export set, for a measured startup cost:
  `mutsu -e 'use Test; plan 1; ok 1, "x"'` runs in 7ms with the list and 93ms
  without it, and every `t/` and roast file would pay that. The module itself
  still runs, the list is only an export-name cache, and
  `test_exports_match_the_vendored_module` re-derives it from `Test.rakumod` on
  every `cargo test`, so it cannot diverge observably and a test says so.
- **Not legitimate.** The JSON interception (E2 below) selects a different
  implementation by module name, and picks its exception type from the *set* of
  loaded module names.

### D4. The exception list shrinks monotonically, or each entry is justified in writing

Rung-3 exceptions are a first-class part of the architecture and are enumerated
here. An entry stays on the list only while a written record states **what makes
rung 2 unreachable** and **what would have to become true to reopen it**. An
entry whose stated rationale has expired is re-decided, not re-cited. Adding an
entry is a user decision (D2); removing one is ordinary work.

Being on the list is not the same as being justified. The list currently holds
two entries, one of each kind:

#### E1. `NativeCall` — a justified rung-3 use

`src/runtime/nativecall*.rs` (~2,570 lines). Measured 2026-08-01, recorded in
[#7560](https://github.com/tokuhirom/mutsu/issues/7560): upstream is 1,483 lines
with 308 `nqp::` references and 76 distinct ops, of which 61 are missing. The
verdict does **not** rest on that op count, which moves as the op layer grows.
It rests on two structural blockers:

1. `use QAST:from<NQP>` — NativeCall builds each `is native` sub's body as a
   QAST tree. mutsu has no NQP, no QAST, and no foreign-language loader.
2. `NativeCall::Dispatcher` is written against MoarVM's dispatch programs
   (`nqp::syscall`/`track`/`guard`/`delegate`/`register`). Exposing an
   equivalent surface is a core VM design change, not a module port.

The record states its own reopening condition (all three of QAST, a
dispatch-program-equivalent API, and the op gap closing), and the third blocker
it originally listed — the parser rejecting `is repr<...>` and the `native`
package declarator — is already gone, fixed as ordinary missing Raku. Stakes are
also asymmetric here: 33 files across the bundled batteries depend on
`use NativeCall`, so a half-working replacement takes the TLS stack and the
database layer with it.

#### E2. The JSON `to-json`/`from-json` fast path — retired 2026-09-12

**Status: the interception this entry recorded is gone**
([#8183](https://github.com/tokuhirom/mutsu/issues/8183)). What survives is
`src/runtime/json.rs` plus `src/vm/vm_native_json.rs` as a **last-resort
provider for `JSON::Fast` alone**: `use JSON::Fast` runs the ordinary module
search first and the native routines answer only when nothing resolves, so an
`-I` / `MUTSULIB` / site-repo copy outranks them (D2 holds). `JSON::Tiny` left
the mechanism entirely — it is a vendored battery and loads like any other
module — the load-order-sensitive exception guess is deleted, and both dispatch
sites now sit after routine resolution, so a resolved def always wins.

**Re-decided 2026-09-13 ([#8226](https://github.com/tokuhirom/mutsu/issues/8226)):
`JSON::Fast` gets vendored, not kept.** D4's rule is that an entry whose stated
rationale has expired is re-decided rather than re-cited, and the measurement the
re-decision needed came out decisively: of the 51 `nqp::` ops upstream
`JSON::Fast:ver<0.20.1>` uses, **42 already worked** — the "~50 missing" figure
this ADR inherited was never measured. The nine that did not (`bindpos`,
`shift_i`, `pop_s`, `push`, `chr`, `p6scalarwithvalue`, `p6bindattrinvres`,
`hash`, `ifnull`) are implemented, along with what they were hiding: a `Uni` that
is a real codepoint store, `nqp::create` allocating storage for the storage
types, `'$!reified'`/`'$!storage'` installs, and an NFG-normalizing
`nqp::strfromcodes`. **13 of the 14 upstream test files now pass against the real
distribution.** The remaining file, `t/01-parse.t`, is held by two blockers with
nothing to do with JSON (a ~20,000-frame recursion that overflows the Rust stack
instead of raising — [#8232](https://github.com/tokuhirom/mutsu/issues/8232) —
and `++$pos` passed to an `int $pos is rw` parameter —
[#8233](https://github.com/tokuhirom/mutsu/issues/8233)), so the
vendoring and this entry's deletion are sequenced behind those; see
`docs/batteries/json-tiny.md`. Until then the provider survives as *scheduled for
retirement*, which is what D4 §exception-list already records it as — not as a
justified rung-3 entry.

The rest of this entry is the record of what was there and why it went.

It was `src/runtime/json.rs` (759 lines) plus `src/vm/vm_native_json.rs` (333),
gated on `use JSON::Fast` / `use JSON::Tiny` (`runtime/runtime_module.rs`),
carried in the parser's export list (`parser/stmt/simple/module_exports.rs`),
and winning over the resolved routine in both the statement path
(`runtime/calls.rs`) and the expression path (`vm/vm_call_func_ops.rs`).

It is a partial exception: the real `JSON::Tiny` **is** vendored, and its
`Grammar`/`Actions` run unintercepted against their upstream suite. Only the two
bare module names are shadowed.

`docs/batteries/json-tiny.md` declared the split permanent on two grounds, and
neither survives as written:

- *The `nqp::` half has expired.* It rests on the real `JSON::Fast` needing ~50
  ops and on the op layer having been "rejected". mutsu has 111 ops now (D5). A
  ~50-op gap is a rung-2 bill, not NativeCall's structural wall. (And the ~50 was
  itself wrong: measured in #8226, the real gap was **nine** ops.)
- *The performance half stands as a measurement and does not support the
  conclusion.* 200 META-shaped documents take ~600s through the real grammar
  against 0.49s native — on a path zef walks for every metadata read. Under D3
  that is an argument for making the grammar engine fast, or for a transparent
  specialization of the vendored module's own code path. It is not an argument
  for name-keyed shadowing.

And the mechanism diverges observably, which is what puts it on the wrong side
of D3: one shared native `from-json` picks its *exception type* from which
module names appear anywhere in the program (`json_tiny_exception_style()` is
`JSON::Tiny loaded && !JSON::Fast loaded`, self-documented as a "best-effort
guess"), so a program that loads both gets `JSON::Fast`-shaped errors from its
`JSON::Tiny` calls. It also jumps the resolution ladder (D2), so no `-I`,
`MUTSULIB` or site-repo copy can override it — the record flags this itself, and
it is a real problem the day an upstream security fix has to reach a user who
cannot rebuild mutsu.

**Decision (user, 2026-09-12):** being slow is not a reason to introduce a
mechanism like this. A battery's selection record does not get to declare a
policy exception permanent; that is what this document is for. The retirement's
real bill (the grammar engine's cost on the vendored module, and deciding
`JSON::Fast` separately since it is not vendored at all) is scheduling, not a
reason to leave the mechanism unexamined.

**Carried out the same day.** Two facts found while doing it are worth keeping,
because both had been load-bearing for "permanent":

- *zef's metadata path was never on this mechanism.* `Zef::from-json` calls
  `Rakudo::Internals::JSON.from-json` — a core Rakudo class, unaffected — and
  zef's only `JSON::Fast` mention is inside a pod block. The sequencing worry
  that kept the entry unexamined did not apply to the path it was named for.
- *The measurement had moved ~48x.* 200 META-shaped documents through the real
  grammar: >600s when the split was recorded, **12.6s** today against raku's
  0.84s. The grammar engine is ~15x off rakudo, not off the scale — a bill worth
  paying on its own terms, which is exactly what D3 says such a measurement is
  for.

### D5. The `nqp::` op measurement, stated accurately

The durable finding of `news/2026-07/nqp-op-layer-measured-and-rejected.md` is
**not** "do not implement `nqp::` ops". It is narrower, and still true:

> **The op set required by a given module is a threshold function.** Implementing
> 80% of a module's ops leaves it dead. So a large module is not reached by
> adding ops one at a time, and "demand-driven, an op at a time" does not
> converge *toward a large module*.

Two consequences the blanket form obscures:

- Ops are worth implementing **when they serve something mutsu already ships**.
  That measurement's own worked example makes the point: `nqp::sha1` — one op —
  served zef's `Distribution.id` and bundled OpenSSL's `dll-resource()`, while
  the "obvious" 42-op `JSON::Fast` target would have unlocked nothing. mutsu has
  since grown 111 ops on exactly that basis.
- Picking a large nqp-heavy module and grinding its op list is the shape the
  measurement rejects. Reaching such a module means clearing its op set as a
  *set* — with the tiers that need compiler lowering (control ops taking thunks)
  and representation work (a null sentinel distinct from `Nil`/`Any`,
  uninitialised P6opaque storage) planned as part of it, not discovered halfway.

Cite this form. The blanket form is superseded by what shipped.

### D6. Retirement is the expected end state of an exception, and it has a known cost

Two worked examples set the precedent, and both are the answer to "isn't rung 2
too expensive":

- **`Pod::To::Text`** ([#5644](https://github.com/tokuhirom/mutsu/issues/5644),
  `docs/batteries/pod-to-text.md`). A native `pod2text` builtin, retired by
  vendoring rakudo's own 168-line module to `modules/Rakudo-Core/` and fixing
  **three general interpreter bugs** — none of them Pod-specific. Output is now
  character-for-character identical to `raku`, including the trailing-newline
  behaviour the native renderer got wrong. The native version had been written
  "the same pattern as JSON::Fast"; the pattern was never transferable, and
  copying it is what this ADR exists to stop.
- **The native `Test` provider** (deleted 2026-09-10,
  `news/2026-09/the-native-test-provider-is-deleted.md`). ~3,300 lines removed —
  `runtime/test_functions/`, `vm/vm_native_test.rs`, the subtest halves, the
  `MUTSU_REAL_TEST` gate, both comparison-sweep scripts, and the seven dispatch
  gates that existed only to keep two providers apart. The final dual sweep
  measured **0 regressions** across 3,950 `t/` files and 1,436 whitelisted roast
  files, with one file passing *only* under the vendored module. What survived is
  the `TEST_EXPORTS` cache described in D3 — an optimization, kept because it
  cannot diverge and a test proves it.

A third case is on the other side of the same coin: the native `monitor`
declarator stopgap (#5640) was retired not by deleting a provider but by growing
the MOP until `OO::Monitors` ran verbatim
(`news/2026-08/exporthow-declare-mop.md`). Retirement means the real module
runs — it does not mean the feature disappears.

## Consequences

- **New batteries have one path.** Rung 1 or rung 2. A rung-3 proposal stops and
  goes to the user, with the exception-list discipline (D4) as the bar it must
  clear.
- **"It is slow" stops being an argument for a mechanism.** It is an argument for
  a benchmark and a scheduled optimization (D3). A change that alters an
  observable — an exception type, a message, an edge-case answer, which module
  resolves — is out of scope for a performance argument regardless of the number
  attached to it.
- **The exception list is auditable.** Two entries today, one justified (E1) and
  one retired the day this ADR landed (E2 — what survives there is a last-resort
  `JSON::Fast` provider, **re-decided 2026-09-13 as scheduled for retirement**:
  the real distribution now runs, 13 of its 14 upstream test files passing, and
  the two remaining blockers are general interpreter gaps, not JSON ones).
  Any change to that list is visible as a diff to this ADR.
- **The `nqp::` op layer is not forbidden and never was.** D5 is the form to
  cite; work that grows ops in service of something mutsu ships needs no
  exemption from a rejection that was never that broad.
- **A battery record cannot make policy.** `docs/batteries/<lib>.md` records a
  selection and its provenance. A deviation from this ADR belongs in a
  superseding ADR, not in a record's prose.

## Alternatives considered

- **Allow native provision behind a measured performance threshold.** Rejected:
  this is exactly what happened by default with JSON, and D3 explains what it
  costs. A threshold also has no natural value — every gap is large until the
  interpreter improves, so a threshold policy converges on reimplementing
  whatever is currently slow, which is the private-dialect outcome by another
  route.
- **Allow native provision with a documented divergence list.** Rejected: the
  divergence list is unbounded and unknowable (it is every behaviour the tests do
  not cover), and it is discovered by users rather than written by us. The JSON
  entry's exception-type-by-loaded-module-set behaviour was not on anyone's list.
- **Patch the vendored source instead of the interpreter.** Rejected by
  BATTERIES.md §3 for a concrete reason: a patched tree cannot be re-vendored
  cleanly, so every upstream bump becomes a merge, and the mutsu-only fix earns
  no compatibility signal and helps no other dist.
- **Leave the policy as prose.** Rejected — this ADR exists because that is what
  produced the E2 carve-out: with no document holding the reasoning, a battery's
  own record was able to declare an exception permanent, and an expired
  measurement kept being cited for two months.

## Implementation status

The policy is in force and is what the codebase already follows. The E2 entry
was retired on the day this ADR landed; what is left of it is a single
last-resort provider whose own justification is still to be written:

| Item | State |
| --- | --- |
| D1/D2 — rung ordering, rung-3 ban | In force since 2026-08-01 (user decision); `BATTERIES.md` §1, `CLAUDE.md` |
| D3 — optimization vs. substitution | Stated here for the first time; no known violation other than E2 |
| D4/E1 — `NativeCall` | Justified exception; reopening condition in [#7560](https://github.com/tokuhirom/mutsu/issues/7560) |
| D4/E2 — JSON interception | **Retired 2026-09-12** ([#8183](https://github.com/tokuhirom/mutsu/issues/8183)); `JSON::Fast`'s last-resort provider remains, **re-decided 2026-09-13 as scheduled for retirement** ([#8226](https://github.com/tokuhirom/mutsu/issues/8226)) — the real distribution runs, 13/14 upstream test files pass |
| D5 — accurate `nqp::` framing | Stated here; 111 ops shipped |
| D6 — retirement precedent | `Pod::To::Text` and native `Test` both retired |
