# TRIAGE — prioritized snapshot of todo/ (2026-09-07b)

A ranked index of every open finding under `todo/tickets/`, `todo/deep/` and
`todo/perf/`, so a session can pick the next unit of work without re-reading
all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

## What changed since the 2026-09-07 regen (`eca048dec`, PR #7456)

Surveyed at `a57ab3eaf`: **58 files** — 24 `deep/`, 19 `tickets/`, 15 `perf/`
(was 67: 30/22/15). The regen itself then retired one closed ticket to `news/`
and filed three new ones; a follow-on doc-diff re-sweep filed four more, while
PR #7506 closed two rows mid-flight. The
tree stands at **62** — 24/23/15.

**One of those four is a Tier S regression that landed on `main` the same day**
— see [lazy-seq-argument-vanishes-into-a-user-slurpy](tickets/lazy-seq-argument-vanishes-into-a-user-slurpy.md),
bisected to PR #7501, CI green.

**48 PRs merged (#7454-#7504)** in the ~20 hours since the last regen, closing
**20 `todo/` files** (6 `deep/`, 14 `tickets/`) and filing **11 new** ones (all
`tickets/`). 44 new `news/2026-09/` entries; that directory now holds 267 files.

- **All five of the last regen's recommended campaigns were started, and three
  finished.** That is the strongest evidence so far that this file actually
  routes work, and it is also why the snapshot decayed so fast.
  - Campaign 1 (ADR-0058's frame bug) — **done**. `deep/deferred-map-callback-
    runs-in-the-consuming-frames-env` is closed; verified here (`g(@sizes)`
    recursion answers `2` on both). **ADR-0058 is now `Accepted — fully
    implemented, steps 0-4 all shipped`**, so one of the two deferral
    mechanisms is retired.
  - Campaign 4 (the mixin cluster) — **done**. All five tickets closed in one
    coordinated pass, exactly as the campaign asked.
  - Campaign 3 (the `Test`-provider retirement) — **advanced hard**, nine
    call-path news entries; criterion 2 is now the only gate left.
  - Campaign 2 (ADR-0068 step 3) — partly landed (`fix/adr0068-step3-routes`).
  - Campaign 5 (ADR-0039 slice 2) — **attempted and withdrawn a third time**
    the same day; only a sub-fix shipped under the PR title (#7485).
- **No new ADRs.** Last cycle wrote seven in a day; this one wrote none and
  *implemented* six (0039, 0053, 0055, 0058, 0073, plus ADR-0024 touched).
  ADR-0058 is now the only recently-authored ADR that is fully closed out.
- **`tickets/` refilled entirely from neighbourhood sweeps.** All eleven new
  tickets came from measuring the shapes next to a fix in flight. The
  `docs/doc-diff-backlog.md` feeder contributed **nothing** this cycle — see
  Housekeeping, where it turns out that feeder has fully drained.

### What was re-verified for this regen

Every one of the 58 files was read end to end, and every runnable repro was
run against `raku v2026.07` on a fresh `target/debug/mutsu` at `a57ab3eaf`.
The orchestrator independently re-ran 18 of the highest-consequence rows.
Findings:

- **Five Tier S rows, up from three**, and four of them are new to this tier.
  Every one is silent data loss, not a crash.
- **The old Tier S headline is gone** and the surviving one got *worse*: see
  the probe-shape note below.
- **Two files' recorded repros are stale in the dangerous direction** — they
  now pass as written, while the bug is still there under a neighbouring
  shape. `tickets/is-type-capture-cell-exclusion-is-by-name-across-the-frame`
  passes with its two blocks in the recorded order and fails when they are
  reversed (`2` vs raku's `4`). A session that re-runs only the recorded repro
  will close a live Tier S bug as fixed.
- **Probe shape decides the answer on the Tier S gc row.** `$h.bag.push(1)`
  from 24 threads delivers 1200/1200 (11 runs, idle and under 16 competing
  busy loops); the file's own `$h.bag[$i] = 1` shape delivers **68-97 of
  1000**. The file records 418/543/304, so the loss has roughly quintupled.
  Same file, same build, same thread count — only the store path differs.
- **Three ADR headers are behind their own bodies**: ADR-0011 (its progress
  note is dated 2026-08-02, five weeks of slices ago), ADR-0065 ("S0 and S1
  shipped" while its phasing table marks S5a done), and **ADR-0070** ("slice 1
  implemented" while §Implementation records slice 2 landed).
- **Three new bugs were found that no `todo/` file covers**; all three were
  reproduced independently by the orchestrator and are filed as tickets (see
  Housekeeping).

**Standing caveat.** A tier is a routing hint. This is one run on one box;
CLAUDE.md's rule still applies — re-verify a ticket's repro on your own build
before acting on it, and read the *tail* of a ticket file, where successive
"Re-verified" sections accumulate and can themselves rot. **This cycle adds a
sharper version of that rule: re-verify the repro's *shape*, not just its
text.** Two of the five Tier S rows were nearly recorded as fixed because the
first probe tried was a near-miss of the one that fails.

## How the ranking works

- **Tier S — Soundness.** Crashes (SEGV/panic/stack overflow), memory
  unsafety, or *silent data loss* — a write that is dropped, or that lands
  somewhere other than where it was aimed, with nothing detecting it. Always
  highest priority regardless of effort.
- **Tier B — Correctness, broad impact.** A wrong answer or missing
  capability in a common construct, or a bug that blocks an entire
  dist/battery. B1 = broad language-construct correctness; B2 =
  batteries/dist-blocking.
- **Tier N — Correctness, narrow impact / diagnostics.** Wrong answer in a
  rare construct with no known blocked test, or a wrong/missing *error
  message*, or mutsu accepting code raku rejects.
- **Perf.** Batched into their own profiling-heavy session; the
  implementation agent for a perf item **runs solo**.
- **Icebox.** Blocked on a design decision or an explicit user call, or a
  pure decision/measurement/cleanup record with no failing repro.

**Effort** (S/M/L/XL) is shown but does not change tier.

---

## Tier S — Soundness (silent data loss, hangs)

| Ticket | Breadth | Effort | Verified 2026-09-07b |
|---|---|---|---|
| [gc-contents-mut-cross-thread-aliased-writes](deep/gc-contents-mut-cross-thread-aliased-writes.md) | ADR-0068 §4 **step 3**: an element store through a container returned by a USER method | XL | **Narrowed to one route, and that route is far worse than recorded.** `class Holder { has @.items; method bag() { @!items } }` with `$h.bag[$i] = 1` from 20 threads delivers **68/78/89/94/97 of 1000** (raku 1000, 5/5 runs) — the file records 418/543/304, so ~92% of writes are now lost. The **discriminator is the store path, not the load**: the *same* accessor with `.push` delivers 1200/1200 at 24-way on an idle box and under 16 competing busy loops (11 runs). Guarded routes hold: the file's named-sub probe answers `1000` 5/5. The diagnosis is intact and is a *decision*, not a patch — the guard is taken after `call_method_with_values` returns, so the accessor dispatch (where `Gc::make_mut` copies the aliased node) runs unguarded, and that funnel's comment deliberately kept it outside to avoid self-deadlock. **One stated blocker is now stale: route 5 is NOT blocked on Channel-supply delivery** — that was fixed by `187fc2eff`, and a 20-value tap now delivers `0..19` in order 3/3. |
| [decimal-literal-with-big-integer-part-loses-it](tickets/decimal-literal-with-big-integer-part-loses-it.md) | every decimal literal whose integer part exceeds `i64::MAX` | **S** | **Confirmed, unchanged**: `1000000000000000000000000000000.5` evaluates to **`0.5`**, silently; `12345678901234567890.5` likewise. Threshold is exactly `i64::MAX` (`…807.5` correct, `…808.5` wrong). One `unwrap_or(0)` at `src/parser/primary/number.rs:473` swallows the `parse::<i64>()` failure that was the only signal selecting the already-correct BigInt arm. Both siblings are clean — the exponent form and the pure-integer form both agree with raku — so the blast radius is that one branch. **Still the cheapest Tier S row this file has ever carried.** |
| [short-rhs-slice-assign-broadcasts-instead-of-padding](tickets/short-rhs-slice-assign-broadcasts-instead-of-padding.md) | every slice assignment whose RHS is shorter than the slice | M | **NEW, confirmed, and it fabricates values that were never on the RHS**: `my %h; %h{(1, 2)} = "z"` writes `"z"` into key `2`; raku writes `Any`. It is a clamped index, not a scalar broadcast — `%j{(1,2,3)} = "z","y"` gives `3 => "z"` (the *last* value repeats). The positional twin drops to `Nil` instead of `Any` (`@a[0,1,2] = "z",` → `["z", Nil, Nil]`), and a typed target must pad with that type's undefined value: `my Int @c; @c[0,1,2] = 5,` → mutsu `Array[Int].new(5, Nil, Nil)`, raku `(5, Int, Int)`. Control holds (RHS longer than the slice truncates correctly on both). Both halves want `Interpreter::unassigned_lexical_value`; the site is the slice arm of `src/vm/vm_var_assign_index_named.rs`. |
| [is-type-capture-cell-exclusion-is-by-name-across-the-frame](tickets/is-type-capture-cell-exclusion-is-by-name-across-the-frame.md) | a mutating closure capture of any `%h`/`@a` whose *name* is used with `is <ContainerType>` anywhere in the compilation unit | M | **NEW, confirmed — but ONLY with the recorded repro's two blocks REVERSED.** As written in the file it now answers `4` on both; put the `my %h is BagHash` block *first* and mutsu answers **`2`** against raku's `4` — the closure's `%h<d> = 4` lands in the callee's own `%h`, silently. That declaration order now matters is itself an unexplained lead worth chasing. The mechanism is intact: the `trait_applied` `HashSet<Symbol>` scan over `OpCode::ApplyVarTrait` (`src/opcode.rs:7243`) still subtracts by bare name across the whole frame, and it feeds `needs_cell_unvouched_containers`. The load-bearing side still works (`my %g2 is BagHash = q => 1, r => 2` → `2` on both), which is why the naive removal is not the fix. **Update the file with the reversed-order repro before anyone re-tests it.** |
| [lazy-seq-argument-vanishes-into-a-user-slurpy](tickets/lazy-seq-argument-vanishes-into-a-user-slurpy.md) | every lazy `Seq` passed to a user `*@a` slurpy | M | **NEW, and the `.grep` face is a REGRESSION on `main`.** `sub f(*@a) {…}; f((1..3).grep(*>0))` binds **0** elements where raku binds 3 — the arguments silently vanish. Bisected by building three commits: `f62654e3d` **3**, `2c7fc1dde` (#7481) **3**, `200923834` (#7496) **3**, **`182178e6b` (#7501 `adr0058-step3b-grep-defer`) 0**, `dccfd1737` 0. CI was green. The `.map` (`[]`), `gather` (`1`) and sequence-operator (`1`) faces are **not** regressions — they answer wrongly at `f62654e3d` too. The discriminator is laziness, not slurpiness: an **eager** `.Seq`, a `List`, a `Range` and a literal list all bind 3, and a non-slurpy `@a` receives the Seq intact. |
| [array-slice-with-a-runtime-empty-reversed-range-hangs](tickets/array-slice-with-a-runtime-empty-reversed-range-hangs.md) | any `@a[0 .. $n]` whose endpoint is a runtime-negative variable | M | **NEW, and it is a HANG, not a wrong answer**: `my @el = (4,); my $e = -1; @el[0 .. $e]` never terminates (killed at `timeout 10`); raku answers `()`. The Range itself is correct on both (`(0..$e).elems` is 0), and the *literal* `@el[0 .. -1]` is rejected by both — which is why it went unnoticed. A process-killing bug on the `0 .. $n-1` idiom. |
| [array-subclass-assignment-in-expression-position](tickets/array-subclass-assignment-in-expression-position.md) | `is Array` subclass instances crossing an assignment | M | **NEW, confirmed — and its stated CONTROL is broken, which is the bigger half.** Headline: `my @a := SA.new(3,2,1,4); (my @b = @a).elems` → mutsu `1`, raku `4`. But the row the file says must stay one element is wrong in *statement* position: `my $c = SA.new(3,2,1,4); my @e; @e = $c` → mutsu `4` / `[3, 2, 1, 4]`, raku `1` / `[[3, 2, 1, 4],]`. In value position that same row is correct on both. So the framing is not "value position is missing the rule" but **"the two paths disagree in both directions"**: `set_local_*` (`src/vm/vm_var_assign_set_local.rs`) decomposes an `is Array` subclass even when it arrives itemized through a `$`. Route both through one itemization-aware helper and add the `@e = $c` row to `t/array-subclass-iterator-override.t`, which does not pin it. |

**Two more files carry S-grade rows inside them** and are ranked lower only
because most of their content is not: `deep/immutable-lvalues-that-mutsu-still-
lets-you-assign-to` (section B1 `@a.list.map({$_=7})` loses the write; section
D truncates a `gather` Seq to `[5]`), and `tickets/subscript-argument-container-
producer` row (c) (`my $bl = { $_ = 9 }; $bl(@b[5])` → mutsu `[1, 2]`, raku
`[1, 2, Any, Any, Any, 9]` — the write is silently dropped).

---

## `todo/tickets/` — the surveyed 19, plus 3 filed by this regen

All nineteen were re-run. **One was CLOSED and has been retired to
`news/2026-09/io-path-methods-test-uses-a-unique-temp-directory.md`**
(`t/io-path-methods.t:23` is now per-PID with a `LEAVE` teardown, and two
concurrent `prove` runs both pass). Four of the remaining eighteen are ranked
in Tier S above; the rest are here. The three tickets this regen filed are in
Housekeeping and are **not** re-listed in the tables below.

### Broad correctness (1)

| Ticket | Tier | Note (verified 2026-09-07b) |
|---|---|---|
| [global-match-scan-enumerates-every-end-at-every-start](tickets/global-match-scan-enumerates-every-end-at-every-start.md) | B1, **M** | **Confirmed**: a code block inside `m:g/…/` runs **12** times against raku's 4; inside `.subst`, 2 against 1. `src/runtime/regex/regex_match_find.rs:287` still does `starts.extend(0..=orig_chars.len())` (and `:258` for the stripped variant). Broad because it is on every `:g` scan, `subst`, `comb` and `split`, and the sibling A17 pin shows a `die` in such a block is the severity ceiling. **The file's own open question is the right first step and is still unanswered**: is the `subst` row two starts, or twice at one start? Answer that before designing; then enumerate which callers actually want overlapping ends. |

### Narrow correctness, diagnostics, permissiveness (11)

> **Two rows that were in this table when it was written are already gone.**
> `anon-role-mixin-has-no-order-and-collapses` and
> `array-and-hash-which-collides-on-a-reused-address` were fixed by PR #7506
> (`f6726723f`, `5ae234f6f`), which merged between the survey commit
> (`a57ab3eaf`) and this file landing — verified fixed here
> (`((1 but "x") but A).^name` → `Int+{<anon|1>}+{A}` on both;
> `[1,2].WHICH eq [3,4,5].WHICH` → `False` on both). This is the routing hazard
> the previous regen named, happening again inside a single session.

| Ticket | Tier | Note (verified 2026-09-07b) |
|---|---|---|
| [mixin-picks-the-wrong-role-group-candidate](tickets/mixin-picks-the-wrong-role-group-candidate.md) | N, **S** | **Confirmed**: with `role Z {…}` and `role Z[::T] {…}`, `(1 but Z[Str]).a` dies `No such method 'a'`; raku `Str`. `src/runtime/types/roles.rs:116` is still a bare `registry().roles.get(role_name)`; two more such sites at `:614` and `:844` want the same audit. The **name** is already right (`Int+{Z[Str]}`), so only attribute/method composition takes the wrong candidate — route it through the existing `resolve_role_candidate`/`role_candidates`, which the class-header path measurably already gets right. Smallest open ticket in the file. |
| [object-hash-key-keeps-its-itemization](tickets/object-hash-key-keeps-its-itemization.md) | N, **S** | **NEW, confirmed**: `my %j{List:D}; my $t = $(1,2); %j{$t} = "x"; %j.keys[0].raku` → mutsu `$(1, 2)`, raku `(1, 2)`. Only the itemization tag on the key handed back is wrong — `.elems`, `.keys[0].^name` and lookup all agree. One unwrap at the key-recording step; the `Scalar` wrapper is the correct *transport* through the four index paths and must stay. Prove the round-trip the file names (`%h{$k}` must still read back, i.e. the `.WHICH` is unchanged by the unwrap) before shipping. |
| [regex-str-should-warn-and-return-empty](tickets/regex-str-should-warn-and-return-empty.md) | N, **S** | **NEW, confirmed**: `(/a/).Str.raku` → mutsu `"/a/"`, raku warns `Regex object coerced to string (please use .gist or .raku to do that)` and yields `""`. It also produces a **wrong answer**, not just a missing warning: `for (/a/,) { say ($_ ~~ $_).raku }` → mutsu `Match.new(…)`, raku `Nil`. Split the two consumers of one stringification: `.gist`/`.raku` legitimately show source text and must keep doing so; `.Str` and string context must warn to stderr with rakudo's wording (`quietly`-suppressible) and return `""`. |
| [mro-roles-adverb-lists-roles-in-declaration-order](tickets/mro-roles-adverb-lists-roles-in-declaration-order.md) | N, **S** | **NEW, confirmed — and the file's premise about `.^roles` is wrong.** `K2.^mro(:roles)` → mutsu `K2,R2,R3,Any,Mu`, raku `K2,R3,R2,Any,Mu`. The file implies `.^roles` is already correct; it is **not** — `K2.^roles` is mutsu `R2,R3` against raku `R3,R2`. That is good news: one ordering bug with two observables, fixable once at the registration order or a shared reversal. Keep the file's caution — measure `class K does R2 does R3` where both define the same method before reversing `parents`, since rakudo refuses that composition and the observable is an ambiguity error, not a winner. |
| [an-undefined-typed-state-scalar-reads-as-nil](tickets/an-undefined-typed-state-scalar-reads-as-nil.md) | N, **S** | **NEW, confirmed**: `sub t() { state Int $u; $u.^name }` → mutsu `Nil` twice, raku `Int` twice. Controls hold (`my Int $z` → `Int`; `state $s` → `Any`; `state Int $x = 0` → `0`), and they are exactly the rows the diagnosis predicts stay correct: `SetVarType`'s `typed_scalar_nil_seed_value` seed is overwritten by the following `StateVarInit`. Reorder the seed after `StateVarInit` rather than threading the constraint into the op; the persistence rows in `t/state-and-our-typed-declarations.t` are the gate. |
| [our-typed-destructuring-and-attribute-declarations-are-accepted](tickets/our-typed-destructuring-and-attribute-declarations-are-accepted.md) | N, **M** | **NEW, confirmed**: `our Int ($a, $b)` and `class C { our Int $.x }` both compile in mutsu; raku SORRYs `Cannot put a type constraint on an 'our'-scoped variable`. **Add the phase row the file omits**: the already-refused scalar spelling `our Int $z` is refused by mutsu at *run* time and by raku at *compile* time. If the class-attribute refusal is moving into the declaration planner anyway, doing all three at compile time is one decision instead of three. |
| [an-our-declared-in-a-never-run-block-is-not-installed](tickets/an-our-declared-in-a-never-run-block-is-not-installed.md) | N, **M** | **Confirmed**: `if False { our $o = 4 }; say OUR::<$o>.^name` → mutsu `Nil`, raku `Any`. rakudo installs a package symbol when the compunit is *compiled*, so the slot exists undefined even though the assignment never ran. The file's predicted mutsu output is slightly wrong (it says "no such symbol"; mutsu prints `Nil` — the missing binding reads as Nil, it does not error). `EndWalker` (`src/runtime/end_phasers.rs`) is the shape to copy, but the walk must track `Compiler::qualify_our_variable_name`'s pseudo-package resolution. |
| [inline-container-trait-declaration-in-an-expression-is-a-plain-hash](tickets/inline-container-trait-declaration-in-an-expression-is-a-plain-hash.md) | N, **M** | **Confirmed, and worse than filed**: `(my %q is SetHash).^name` → `Hash` (raku `SetHash`), and `~~ SetHash` is `False`. Beyond the file: `(my %u is MixHash).^name` answers **`Hash[MixHash]`** — a *third*, different wrong answer, so the un-coerced value is not uniformly a bare `Hash`; check that arm separately. `(my @a is Buf).^name` → `Array` while the statement form is correctly `Buf`. **The file's cross-reference is a dead link** — its target was renamed to `is-type-capture-cell-exclusion-is-by-name-across-the-frame.md`. |
| [roles-not-transitive-is-ignored-for-builtin-types](tickets/roles-not-transitive-is-ignored-for-builtin-types.md) | N, **M** | **NEW, confirmed**: `1.^roles(:!transitive)` → mutsu `Real,Numeric`, raku `Real`. `collect_roles_for_class` applies `non_transitive` only when walking the registry's `role_parents`; the built-in role table is flat. **`:local` needs no work** — `1.^roles(:local)` already agrees, which answers the file's open question. Do not test with `Str` (its closure is one deep, so it cannot show the bug); use `Int`, and check `Num`/`Rat`/`Array`/`Hash`. |
| [immutable-list-element-write-is-silently-dropped](tickets/immutable-list-element-write-is-silently-dropped.md) | N, **L** | **Confirmed**: `$l[0].mut` on `my $l = (1,2)` with a `\S:` raw invocant lives and changes nothing; raku dies `Cannot modify an immutable Int (1)`. The write is correctly dropped — only the *diagnostic* is missing, because the binder never consults readonly-ness for a raw invocant. Note the passing controls A2-A4 also differ in wording (mutsu names the `List`, raku the `Int`); fold that into the same change. ADR-0067 rows L4/L5/M1/M2, and the same family as the subscript ticket below. |
| [subscript-argument-container-producer](tickets/subscript-argument-container-producer.md) | N (row (c) is **S**), **L** | **Headline correctly self-declared CLOSED** and re-verified — all six "now works" rows give `[9, 2]` on both. Three residue rows still reproduce: (a) `$obj.^lookup('m')($obj, $c.v)` — a `Method` invoked as a code value counts its invocant as positional 0 (**the file's row as written is under-specified; the method must take an `is rw` parameter or you get an arity error instead — rewrite it before re-measuring**); (b) `sub g(:$y is rw)` accepted where raku SORRYs — a pure parser-validation gap with nothing to do with containers, **split it out as a cheap standalone slice**; (c) an out-of-range subscript argument, where `$bl(@b[5])` silently drops the write. |

---

## How to work `todo/deep/` — by ADR cluster

**Do not run `deep/` oldest-first** (filing order is an accident of which
campaign ran last, and `ls -tr` mtimes are corrupted by worktrees). Work it by
ADR cluster: most deep findings wait on a *slice of an ADR that already
exists*, and one landed slice closes several rows. Every `Status` line below
was read on 2026-09-07b.

| ADR | Status | Rows it would close |
|---|---|---|
| [ADR-0068](../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md) cross-thread container writes | **`Accepted`; §4 steps 1-2 implemented, step 3 started.** §8 (route 3) and §9 (retiring the earned mitigation) are the newest sections; the chained-subscript store landed as `35e8de6e4`. | The remaining Tier S row, now narrowed to a **single** route. Still the only ADR that owns a Tier S row. |
| [ADR-0058](../docs/adr/0058-map-grep-produce-a-deferred-seq.md) map/grep produce a deferred Seq | **`Accepted` — fully implemented, steps 0-4 all shipped 2026-09-07.** §8/§9 record what step 0 measured and which premises did not survive. | **Closed.** Its residue file is gone; what remains is the ADR-0058 decision embedded in `immutable-lvalues` section B1. |
| [ADR-0039](../docs/adr/0039-container-lexicals-resolve-lexically.md) container lexicals resolve lexically | Slice 1 landed 2026-08-20; **slice 2 attempted and withdrawn a THIRD time on 2026-09-07**. Only repair 5 shipped (`442a3a993`, under a PR titled "slice2"). | `deep/adr0039-slice2-container-reads-compile-to-a-slot`, which is now an accurate same-day withdrawal record. |
| [ADR-0073](../docs/adr/0073-regex-atom-candidates-are-demand-driven.md) regex atom candidates on demand | **`Proposed`; slices 1 and 3 implemented, and slice 2's *ratcheted* half implemented** 2026-09-07. Slice 2's non-ratcheted half is open. | `deep/ordered-alternation-eager-candidate-enumeration` **is** that open half. The ADR and the todo file agree exactly — unusual here, and worth trusting. |
| [ADR-0067](../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md) a routine hands back its container | Accepted; every slice implemented. | Residue is three rows inside `tickets/subscript-argument-container-producer` plus `tickets/immutable-list-element-write-…`. The two share one decision: does the `is rw`/topic binder accept a deferred vivification token, or refuse? |
| [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) closure free vars bind their own | Slices 1 + 1b landed (1b's parameter carve-out retired the same day, §7.7); slices 2-5 open. §7.6 records that there are **three** closure-env merges, not two — count `eval_map_over_items`' inline fast path. | `deep/call-compiled-closure-…` (structural only — its headline repro now answers `OUTER` on both, matching the file's own retraction). |
| [ADR-0047](../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md) type identity | Partially adopted — P1/P2 landed; **P3 and P4 not started** | `subtest-compiled-dispatch-async-middleware-regression` (P4 is the prerequisite for re-landing #6499, *not* itself the fix). Cannot land until P3/P4 do. |
| [ADR-0070](../docs/adr/0070-native-methods-declare-the-named-arguments-they-accept.md) native methods declare accepted nameds | **Header says "slice 1"; its own §Implementation records slice 2 landed 2026-09-07.** Sweep is at **18 of 1 422** probes (raku's own baseline is 23). | `deep/native-method-accepted-named-declarations` — hardening, not a live bug. |
| [ADR-0065](../docs/adr/0065-language-server-targets-ai-agents.md) LSP targets AI agents | Accepted; header behind its own phasing table (S2 routine half, S3, S4, S5a all done). **S5b is the only unstarted row.** | `lsp-references-needs-a-side-table-not-ast-spans` (amends D6/S5b; blocked on a *measurement*, not a design). |
| [ADR-0019](../docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md) resolver core | Accepted/implemented; all four completion gates closed. E2b is explicitly demoted to a monitoring signal. | `adr0019-e2-e4-resolver-core` — Icebox, but its counter has **drifted up**: 648 `native_call_unmodeled` on `t/` alone today vs ~400 recorded for `t/`+roast. |
| [ADR-0029](../docs/adr/0029-exception-class-role-membership.md) exception class role membership | Slices 1-3 + R1-R4 landed; **R5 blocked on `vendor-real-test-module`** | `exception-class-hierarchy-is-mostly-unregistered` — all 373 rakudo `X::` subtypes now match and the original headline repro does not reproduce. It is a cross-reference, not work. |
| [ADR-0048](../docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md) placeholder scope | Accepted; P1-P4 landed, P5's scope half landed, value half deferred | `role-body-placeholder-mu-supply` — and the file argues against doing it: corpus hits are zero and rakudo's behaviour is a `VMNull` artifact. |
| [ADR-0059](../docs/adr/0059-is-rw-routines-return-a-container.md) / [ADR-0051](../docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md) / [ADR-0021](../docs/adr/0021-argument-namedness-is-a-call-site-property.md) / [ADR-0025](../docs/adr/0025-captured-scalar-cells-value-kind-blind.md) | Slice 3 / P2+P5 / P5 / slice 3 open respectively | Nothing in `todo/` names them. Open slices with no failing repro — do not resource them ahead of a Tier S row. |
| [ADR-0050](../docs/adr/0050-block-routine-ness-is-a-definition-site-property.md) / [ADR-0043](../docs/adr/0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md) | Both `Proposed`, implementation not started (ADR-0043's Decision 1 is probe-verified and ready) | Nothing in `todo/` — designs waiting for a consumer. |

### Recommended next campaigns

1. **The silent-data-loss sweep — three Tier S tickets, S+M+M effort, one
   session.** `decimal-literal-…` (a one-line routing fix in the parser),
   `short-rhs-slice-assign-…` and `array-subclass-assignment-…` are
   independent of each other and of any ADR, and every one is a *wrong value
   with nothing detecting it*. This is the highest value-per-hour work in the
   file and it did not exist as a cluster before this regen. Do
   `is-type-capture-…` in the same session only if you first replace its
   recorded repro with the reversed-order one.
2. **ADR-0068 step 3 — one route left, and it is now a decision.** The gc row
   has gone from "four measured routes" to exactly one: an element store
   through a user-method-returned container, losing ~92% of writes. **Do not
   start with a site sweep** — the ADR rejects that on the record, three
   funnels are already guarded, and the measured discriminator is the *store
   path* (`.push` is clean, `[i] =` is not). The blocking question is stated
   precisely in the file: the guard is acquired after the accessor dispatch
   returns, and moving it inward risks the self-deadlock that funnel's comment
   was written to avoid. **Route 5 is now unblocked** (its Channel-delivery
   blocker is fixed) — but write its probe only after the new `done`-callback
   ticket lands, since a tap that never signals `done` cannot be measured.
3. **`deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to` — 17 of 17
   rows reproduce byte-for-byte**, which after this cycle's stale-repro count
   makes it the most trustworthy deep file in the directory. It also contains
   two S-grade rows. **Read its "Corrected blocker attributions" section
   first**: three stated blockers were each measured wrong, and the obvious
   runtime rule (`!item.is_container_ref()`) was measured to convert every
   section-B row into a spurious throw. Cheapest order is the file's own: C2
   (`@a[1][0] = 9` autovivifies over a defined `Int` — one `needs_viv`
   predicate, and the sibling `root_needs_viv` already has the right shape),
   then E (refuse associative subscripting on `Seq` at the protocol level),
   then A6 (mark the placeholder block's param readonly — but read the
   `pointy_alias_param` comment; injecting `MarkReadonly` into the body has
   leaked through the native map/grep/first loops and reached CI before).
4. **Finish the `Test`-provider retirement (a PLAN §1 goal, not perf polish).**
   `deep/vendor-real-test-module` now has **correctness criteria 1 and 3 met
   and criterion 2 as the only gate**: 234 580 Ir per assertion after eight
   slices (−52.3% since the fourth pass opened), `write-int.t` at ~28% under
   its 30 s budget, both sweeps at zero regressions. One more slice the size of
   #7459 closes it. **Quote the ratio, never a bare second-count** — the file
   itself had to recalibrate every wall-clock figure when its box changed, and
   ~44.6k of the differential is one-time JIT compilation charged to the extra
   assertions, so steady state is ~190k.
5. **ADR-0039 slice 2 — third withdrawal, and the file says what to do
   differently.** Do not hand-reduce zef source again. **Rebuild the
   `MUTSU_SLOT_READ_FILTER`/`MUTSU_SLOT_READ_DUMP` delta-debugging harness
   first** (it was withdrawn with attempt 3 and no longer exists in the tree),
   then delta-debug the ~92 names. The single open blocker is one container
   (`@prereq-candidates` in `Zef::Client!find-prereq-candidates`) whose
   owner slot is empty while the `__mutsu_atomic_arr::` lane probe wins, so a
   `gather`-body append never reaches the cell; the unreached next step is to
   trace which env tier that `gather` body runs against. All four acceptance
   pins pass today (82 tests), so the gate is a ~10 min battery run, not `t/`.

**One measured process exception, kept from previous regens:** when a change
alters a *universal property of values* ("what is in every container"), run the
full local `make roast` before pushing (ADR-0040 slice 2 needed 17
counter-current fixes, 9 found only by roast). Campaign 5 is exactly that
shape. Ordinary parser/operator/dispatch fixes still delegate to CI.

**Methods worth copying.** Five, three of them earned this cycle:

- **Re-verify the repro's SHAPE, not just its text — this cycle's headline
  lesson.** Two Tier S rows were nearly recorded as fixed: the gc row passes
  1200/1200 with `.push` and fails 68/1000 with `[i] =`; the capture row passes
  as written and fails with its two blocks reversed. A near-miss probe is
  indistinguishable from a fix, and it is *more* dangerous than no probe,
  because it produces a confident green.
- **A neighbourhood sweep after a fix is now the only ticket feeder that
  works.** All eleven new tickets came from measuring shapes next to a fix in
  flight; `docs/doc-diff-backlog.md` contributed zero. Budget the sweep as part
  of the fix and write the neighbourhood list into the ticket.
- **Prove the *shape* of a divergence with a control table before writing a
  root cause.** Repeatedly load-bearing this cycle: `roles-not-transitive`'s
  `:local` control answered the file's open question for free; `mro-roles-
  adverb`'s `.^roles` control *disproved* the file's premise and halved the
  work; `array-subclass-assignment`'s control turned out to be the bigger bug.
- **ADR-0068's stress harness — but its own file now says oversubscription is
  NOT the discriminator, and this regen confirms that.** 24-way on 12 cores,
  idle *and* under 16 competing busy loops, gives 1200/1200 on the clean store
  path; the failing path fails on an idle box. **The discriminator is which
  store path the workload takes.** Negative results still on the record:
  `memcheck` serializes threads and finds nothing; helgrind was a dead end.
- **The two-breakpoint gdb oracle.** Break on the unsynchronized site and on
  the synchronized lane; `already hit N times` on the first with nothing on the
  second means the workload is *exposed*. Deterministic, one debug run, and it
  settles a route in minutes where the stress harness needs hundreds of runs to
  say the same thing probabilistically.

---

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness

| Ticket | Effort | Why here |
|---|---|---|
| [immutable-lvalues-that-mutsu-still-lets-you-assign-to](deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md) | L | **17/17 stated rows reproduce byte-for-byte** — the most trustworthy deep file in the directory this pass, and it carries two S-grade rows (B1 loses a write, D silently truncates a `gather` Seq to `[5]`). Section A (6 rows) is `my @a := (1,2,3); @a.map({$_=5})` answering `(5 5 5)` where raku throws; A6's discriminator is verified (the **pointy** twin `-> $x { $x = 9 }` correctly throws, so `pointy_alias_param` really is it). The hash-pair sub-section is inverted — mutsu throws `X::Assignment::RO` where raku answers `{a => 9}` — with both controls agreeing, which cleanly confirms the producer split. See campaign 3 for the order. |
| [adr0039-slice2-container-reads-compile-to-a-slot](deep/adr0039-slice2-container-reads-compile-to-a-slot.md) | XL | **No failing repro of its own — it is the enabler**, and an accurate same-day record of the third withdrawal (2026-09-07). `src/compiler/expr.rs:145` still emits `GetArrayVar(name_idx)` where a scalar read compiles to `GetLocal(slot)`, and *that by-name read is what hides store-lane bugs*: two paths naming different containers under one name look fine as long as every read re-resolves. Six repairs are unlanded and one blocker is unsolved. Nothing in the file is stale. See campaign 5 for the one procedural change that matters. |
| [global-match-scan-enumerates-every-end-at-every-start](tickets/global-match-scan-enumerates-every-end-at-every-start.md) | M | Ranked in the tickets table above; repeated here because its breadth (every `:g`, `subst`, `comb`, `split`) is the widest of any open correctness row outside Tier S. |

### B2 — batteries / dist-blocking

| Ticket | Blocks | Effort |
|---|---|---|
| [vendor-real-test-module](deep/vendor-real-test-module.md) | making the vendored upstream `Test` the default (retiring a rung-3 native provider — a PLAN §1 goal) | L. **Criterion 2 is the only gate left**; criteria 1 and 3 are met (roast 1432 pass-both / 0 regressed; `t/` 3760 pass-both / 0 regressed). **234 580 Ir per assertion** after eight slices, −30.2% this pass and −52.3% since the fourth pass opened at 492 188. `write-int.t` idle median 7.5 s, 10.9 s under `prove -j4` (~22 s scaled to the reference machine, ~28% under the 30 s budget). The module is unmodified (`md5sum` matches the file exactly) and `MUTSU_REAL_TEST=1` output is byte-identical to the native provider. **Read the ratio, not the second-count**, and remember ~44.6k of the differential is one-time JIT compilation. |
| [config-toml-battery-core-blockers](deep/config-toml-battery-core-blockers.md) | `Config::TOML` (11/19) + `Crane` (4/15) battery slot | L (cluster). **Re-measured exactly this pass**: both counts and the failing-file lists match the file item for item, which makes this the freshest file in `deep/`. **Do not start the vendoring steps** — `Crane` at 4/15 is too thin. Cheapest next unit is the List-`splice` dispatch shape (mutsu raises `X::Immutable` where raku has no `List` candidate and raises `X::Multi::NoMatch`; Crane's `CATCH` maps only raku's spelling). **Two listed residues are narrower than written** — the `*-0` splice bug and the `Array:D` object-hash key both agree with raku standalone, so they need the dist context; do not chase them with a reduced repro. |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | the template battery runner-ups | XL (cluster). **`Template::Nest::Fast` closed 0/10 → 10/10 on 2026-09-07**, joining `Template::Mustache` 13/13 and `Template6` 12/12. Remaining: `Template::HAML` 39/83 (**now the largest prize**, and recorded as 2-3× slower to load than raku — the file points that half at a `tickets/` split-out that **no longer exists**, so re-measure the load gap before treating it as tracked elsewhere), `Template::Jinja2` 3/23, `Template::Mojo` 4/5, `SP6` 10/11 (parity), `Template::Classic` 0/1. **`Template::Classic`'s recorded diagnosis does not reproduce reduced** (its `$<part> = <rule>`-in-a-`\|\|`-chain shape matches on both) — re-fetch and re-reduce that dist. Every row closed so far was closed by a *general* interpreter bug found by deleting constructs, never by the recorded symptom. |
| [p5tie-stash-bind-key-protocol](deep/p5tie-stash-bind-key-protocol.md) | `P5tie`, `annotations`, transitively the `Tie::*` dists | L. **Confirmed**: `Stash.^can("BIND-KEY")` → mutsu `False`, raku `True`; the P5tie call shape dies `No such method 'BIND-KEY'`. **The file understates the work**: mutsu has no `PseudoStash` type at all (`CALLER::.^name` → mutsu `Stash`, raku `PseudoStash`, verified), so the package-stash case and the lexical-pad case are the *same* value today and must diverge first. Capped by the file's own corpus measurement (~0.5% of sampled dists). The `ContainerRef` substrate it waits on is **available** — ADR-0013 §7's refinement landed — which the file does not say. |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | re-landing #6499's `subtest` perf win | L — `tap_subtest.rs:106` still routes through `call_sub_value` unconditionally, so the revert stands. Class registration, `LEAVE` phasers, async transforms and a port cascade are **all ruled out by the file's own A/B probes**; the surviving shape is early/conditional-response middleware. Bisect from the dispatch end with `rust-gdb` frame diffs, not from Cro. **Cannot land regardless until ADR-0047 P3/P4 start.** |
| [rakuast-remaining](deep/rakuast-remaining.md) | RakuAST parity | XL — an actively-worked ledger whose open items all still reproduce. **Its own count is understated: 113 `t/rakuast*.t` files and 921 assertions today, against the 93/646 recorded on 2026-09-02.** The file correctly names associative subscripts as the highest-leverage remaining item (it blocks both the read gap and the matching `EVAL` item). Zero roast dependents; pick by user impact and read the RakuAST skill first. |
| [unify-statement-expression-control-construct-compilation](deep/unify-statement-expression-control-construct-compilation.md) | nothing directly — architectural debt | XL, and **measurably worse for the third consecutive regen**: `stmt.rs` **5191** lines (recorded 4451), `helpers_do_expr.rs` **666** (476 → 609 → 666), `ForLoopSpec` **30** `pub(crate)` fields (21 → 27 → 30), still constructed in exactly two places and still six `compile_do_*` entry points. Refresh those three counts when you touch it — they are the file's whole argument. No cheap next step by construction, but a shared `ForLoopSpec` builder helper is a small PR that buys most of the parity guarantee. |

---

## Tier N — narrow correctness / diagnostics

| Ticket | Category | Effort / note |
|---|---|---|
| [ordered-alternation-eager-candidate-enumeration](deep/ordered-alternation-eager-candidate-enumeration.md) | correctness-narrow | L — **this is ADR-0073 slice 2's non-ratcheted half**, and all three open rows reproduce exactly: E1 `<part> 'c'` runs the block **5** times against raku's 2, E2 (three-level) `5` vs `2`, E6 collects `one,two` where raku commits to `one`. The 87-test pin passes with 5 `todo` markers. **The match result is correct in all three — only a side-effecting block runs extra times**, so promote it to B1 only if a real grammar is found where that is observable. The separable sub-case the file names (no arguments, no proto, one resolved candidate, not LR-active) closes E1/E2 without touching the seed loop; E6 is the harder half. |
| [module-toplevel-private-sub-leak-cleanup](deep/module-toplevel-private-sub-leak-cleanup.md) | accepts code raku rejects | L — **confirmed, and its GATE is now GONE.** `require HelperMod; helper()` prints `mod helper` in mutsu where raku SORRYs `Undeclared routine`. The blocking ticket (a bare `Pkg::name` falling back to the caller's own sub) was **fixed** by #7458 — both implementations now die `Could not find symbol '&zzz' in 'GLOBAL::NoSuchPkg'` — so the assertion this wants to pin is finally writable. **A full implementation exists**: PR #7436 (still closed-not-merged, +556/-137), and its branch survives on `origin`. Read `gh pr diff 7436` rather than re-deriving; its design (seclusion is a *move* into a per-compunit store, not a delete) is sound and only the last mile is open. |
| [native-method-accepted-named-declarations](deep/native-method-accepted-named-declarations.md) | accepts code raku rejects | M — **no longer a live bug; it is hardening.** The sweep measures **18 of 1 422** probes exactly as the file claims, and `t/native-method-accepted-nameds.t` is 123/123. What is left is `subst`/`trans`, which read adverbs out of a slurpy (`trans` declares nothing but the implicit `*%_` and still reads `:d`/`:s`/`:c`, which is why "the only slurpy is `%_`" cannot be used as a rule). **Section 3 needs a rewrite**: two of its rows are now stale (`Any.first(1)` and `Any.values` agree), and one is *worse* than recorded — `{ $_ }.returns(:qqzz9)` answers `&<composed-method:returns>` where raku answers `(Mu)`, i.e. passing the adverb turns the method into its own code object. |
| [metamodel-roles-are-not-composable-types](deep/metamodel-roles-are-not-composable-types.md) | missing feature | L — **confirmed, headline and both deferred residues.** `class WithStashHOW does Metamodel::Naming { }` dies `X::InvalidType` before the body is read; raku composes it. The renaming preamble is the point worth keeping: **three** framings were closed under this slug, each measured wrong or already fixed before the next was written, and what survived was the aside. Both residues (`.^add_method` reporting the added name; a plain `sub` as a method not taking the invocant) are correctly marked *deferred on corpus evidence* — every `.^add_method` in `roast/`, `vendor/` and `modules/` passes a method — so do the headline, not them. |
| [slurpy-hash-named-arg-raku-boolean-shorthand-missing](deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) | rendering | L — **confirmed on every row**: `foo :a1:b2` renders `{:a1(Bool::True), :b2(Bool::True)}`; raku `{:a1, :b2}`, and `False` renders `:!a`. Its own recommendation ("do not fix `Hash.raku` in isolation — it wants per-element containers") is right, and the mixed case proves the shortcut wrong. **Add the trap this survey found**: the file's `.VAR.^name` discriminator now *appears* to pass, but coincidentally — mutsu reports `Bool` for **everything** inside a slurpy-bound hash, so `%h<c>.VAR.^name` is `Bool` where raku says `Scalar`. A future session running only that probe will wrongly conclude the containers landed. |
| [begin-time-adverb-value-interpolation](deep/begin-time-adverb-value-interpolation.md) | correctness-narrow | L, **low priority by its own assessment** — `my $a:foo<42> = "answer"; say $a:foo«$c»` gives `Nil` where raku resolves it, and it parses cleanly, silently resolving to a different name. No roast coverage (`S02-names-vars/varnames.t` tests literal values only and passes). The honest cheap improvement is step 4 alone — make an undeclared/non-constant interpolation a **compile error** instead of a silent `Nil` — which needs none of the whole-AST name-normalization pass. |

---

## Perf — batch into one profiling session; implementation agent runs SOLO

### **Correction: the raku ratio does NOT normalize the CI runner's host class.**

This is the most important perf finding of the cycle and it contradicts a
premise this file and CLAUDE.md have both been carrying. The bench CI runs on
a **bimodal** runner pool. Splitting the last 60 main-push rows by
`bench-startup` `mutsu_median_s` (`< 6 ms` = fast host):

| | n | mean `bench-ctor` **ratio** |
|---|---|---|
| fast-host runs | 16 | **0.62** |
| slow-host runs | 44 | **0.88** |

`hash-access` stays 0.14-0.19 and `fib` 0.30-0.47 across the same swings, so
the effect is specific to the long, allocation-heavy OO benchmarks — **and it
is in the ratio column, not just the seconds column**. Consequence:
`bench-ctor`, `method-call`, `bench-class`, `time-parts`, `debug-guard`,
`bench-mandelbrot`, and the *interpreter* rows of `bench-fib`/`bench-tak`
**cannot resolve anything smaller than ~30%**. Always quote the benchmark name
*and its noise class* with any number.

**Nothing moved between `ce6b0576f` and `a57ab3eaf`** (~10.6 h, 49 commits).
Two apparent moves (`bench-ctor` 0.97 → 0.74, `method-call` 0.88 → 0.795) are
fully explained by host-class composition (4/20 fast-host runs in the earlier
window, 7/20 in the later one).

Current ratios at `a57ab3eaf` (2026-09-07T13:31:55Z), interpreter row then
`+jit`, **usable signals only** (±0.02 at median-9): `fib` 0.44 / 0.23,
`hash-access` 0.17 / 0.17, `bench-hash` 0.18 / 0.18, `bench-array` 0.11 / 0.12,
`string-concat` 0.10 / 0.10, `bench-startup` 0.04 / 0.05, `poly-call` 0.35 /
0.37, `word-count` 0.44 / 0.44, `bench-string` 0.30 / 0.30, `array-ops` 0.36 /
0.37, `bench-grammar-parse` 0.06 / 0.05, `bench-tak+jit` 0.59. **`time-parts`
(1.02) is the only series still above 1.0** — the `bench-ctor` "only benchmark
slower than raku" headline is obsolete.

**Two real steps landed just before the last regen and were never recorded:**
`bench-string` 0.44 → 0.29 (−34%, clean and sustained, at `d4a63aebb` / PR
#7372), and `bench-yaml-parse` daily-min 0.798 s → 0.090 s (**~7.5×**, at
`8607293b0`), while `int-arith`'s daily min stayed flat — i.e. the yaml
ticket's claimed 8.3× is CI-confirmed.

| Ticket | Status |
|---|---|
| [closure-sequence-evolution-performance-gap](perf/closure-sequence-evolution-performance-gap.md) | **RE-DIAGNOSED — its own root-cause section is wrong, and the replacement is deterministic.** `max :by(&fitness)`, which the file blames first and names `builtins_collection_extrema.rs` for, does **zero** per-call compiles (`add_constant` is 27 at N=5 and 27 at N=10). The entire per-iteration compile is the inline `<?{ rand < $chance }>` code assertion inside `subst` (+60 constants per `subst`, +690 per generation), with **300 full by-name resolutions of `rand` per generation** and `env_deep_copies` +88/iteration. The regex *parse* is already cached, so the cost is downstream: `eval_regex_code_block_body` (`src/runtime/regex/regex_eval_repeat.rs:227`) snapshots the whole env into a `HashMap<Symbol, Value>` and then walks it again for rebindings — O(env) twice per assertion evaluation. Delete `builtins_collection_extrema.rs` from the file. **Highest ratio of known-cause to remaining-unknown in the directory.** |
| [non-constant-defaults-still-forfeit-the-light-path](perf/non-constant-defaults-still-forfeit-the-light-path.md) | **ACTIVE, and the cheapest triage here — the deciding measurement is one sweep with a counter that already exists.** `record_param_default` (`src/vm/vm_stats.rs:939`) discriminates cleanly: `sub f($x, $y = $x + 1)` ×2000 → `evaluated=2000 constant=0`; 200 real-`Test` `ok` assertions → `evaluated=0 constant=200`. So tiers 1+2 landed and `Test` is fully on the constant path. Tier 3 (compiling default *expressions* into a callee prologue, retiring `eval_param_default`'s compile-afresh-on-every-call at `binding_helpers.rs:72`) is a compiler change. **Sweep `t/` + the roast whitelist and total `evaluated=` before building** — the file's own instruction is to close itself if that is a thin tail. `is copy`/coercion params are a separate axis; do not fold them in. |
| [adr0019-g3-diffuse-bless-allocation-cost](perf/adr0019-g3-diffuse-bless-allocation-cost.md) | **ACTIVE — the only file with a working, load-independent attribution tool pointed at a named, unexamined region.** `mfast:epilogue` is fixed (4.9 → 2.2 allocations/call); `bless:named-args` was **misdiagnosed and closed out** — sub-scoping attributed all 11 allocations to the `%`-sigil coercions and **zero** to the guessed linear scan, so indexing would have bought nothing. What is left: the locals-init `format!`, and **`mfast:body` at 40.7 allocations per method call exclusive, by far the largest region and entirely unexamined**. Sub-scope it by opcode family before saying anything about it. Fold `bench-ctor-construction-parity` into this file. |
| [hash-access-diffuse-regression-2026-09](perf/hash-access-diffuse-regression-2026-09.md) | **ACTIVE, and the best-instrumented file in the directory.** Ratio 0.17 today. Leads are structurally live: `var_default(&self, name: &str)` (`runtime_var_meta.rs:472`) still has no `_sym` twin, and `get_env_with_main_alias_inner` (`vm_env_helpers.rs:1123`) is still the `&str`-keyed chokepoint. Concrete slice: four interns → one in `try_fast_hash_element_assign` (S). Its opening warning is the model for the whole directory — **read the ratio column, not `mutsu_median_s`**; a 26% "step" was runner noise that moved raku's baseline identically and cost two release builds. The SipHash→FxHash swap **needs an ADR** (HashDoS on user-controlled keys). Merge `hash-workload-cost-…` into this file. |
| [late-august-call-path-slowdown-remainder](perf/late-august-call-path-slowdown-remainder.md) | **ACTIVE — the campaign's central ledger, ~8 items closed with news entries.** But ADR-0066 is now **Accepted and implemented**, so the "biggest single remaining item" is landed and the post-09-03 profile table must be re-taken before picking anything. Open: item 1's locals half (same object as `locals-frame-is-a-pooled-vec`), item 2 (readonly bookkeeping, ADR-scale), item 3 (`mutsu_jit_1` ~50% slower since August, unexplained — dump the generated code for both builds), item 4 (75 `env().get("<literal>")` sites), item 5 (984-byte stack frame). **Its "Method notes" section is the canonical measurement protocol for this directory** — interleave A/B binaries; measure *retired instructions*, not cycles, when the touched code is not executed by the benchmark; pick a control benchmark; and **run the correctness gate BEFORE the A/B** (a change that accidentally skips work measures faster — #7269 read −12.2% and the honest number was −8.2%). |
| [hash-copy-allocates-a-string-per-key](deep/hash-copy-allocates-a-string-per-key.md) | **CONFIRMED structurally** (filed under `deep/` because the fix is a type change across the codebase; it belongs in `todo/perf/`). `src/value/mod.rs:1095` — `HashData { map: HashMap<String, Value>, original_keys: Option<HashMap<String, Value>>, … }` under `derive(Clone)`, so every value-copy allocates one `String` per key, twice over. Raku assignment semantics make hash copies routine. Its most useful content is the **negative** result that produced it (the guessed linear scan allocated zero). `Arc<str>` is the likely answer, not `Symbol` — arbitrary runtime keys would grow the symbol table without bound — and `Borrow<str>` survives it. **Instrument `HashData::clone`/`detached_container_copy` and run `bench-hash` / `bench-yaml-parse` / the zef `Ecosystems` populate path before committing.** |
| [listop-call-bypasses-every-compiled-call-cache](perf/listop-call-bypasses-every-compiled-call-cache.md) | **MEASURED-DEAD for its headline; ACTIVE for item (c).** The structural claim verifies (`execcallpairs:carrier=200` of 200 assertions, reproduced at HEAD) but the file itself measured the whole carrier arm at **1.2%**, and `multi sub ok(Mu $cond, $desc = '')` is ineligible for both light paths so a cache hit lands where the carrier already lands. Its 8.53% row is fixed (−7.5% wall clock). **Do (a) only as a slow-path retirement, not expecting a speedup — the file says so explicitly.** The real item is (c): `bind_function_args_values` at 13% and ~47 `format!` per assertion, **each larger than (a) and (b) combined**, and neither attributed yet. |
| [interpreter-call-path-in-hot-loops](perf/interpreter-call-path-in-hot-loops.md) | **STALE-NUMBERS — its newest diagnosis is dead, for the THIRD consecutive framing.** Re-ran its own instrument at HEAD (`MUTSU_REAL_TEST=1 MUTSU_VM_STATS=1`, 200 `ok`): `function-call opcodes=1218 interpreter_fallbacks=0 (0.0%)`, `function-full-resolve total=15` (constant, not per-assertion; `ok=2`), and the `nqp::` rows are **gone entirely**. The file records `interpreter_fallbacks=10012 (83.3%)` and `total=50060` with ~25 resolves per assertion. Its whole numbered plan is moot; the 2026-08-29 `&`-sigil section was already retracted by the 2026-09-04 one that is now itself dead. **Do not read anything below its title as current.** Presumably still true is the *symptom* (vendored `Test` ≫ raku per assertion) — but nobody has re-measured that either. Keep its benchmark trap: always return and print the accumulator, or raku's optimizer deletes the loop. |
| [method-dispatch-flattens-the-env-on-every-call](perf/method-dispatch-flattens-the-env-on-every-call.md) | **MEASURED-DEAD for the obvious fix — do not pick this up as an implementation task.** The relocation was implemented in full over 44 sites and reverted: as-committed 158.8 M Ir / 0.326 s, flatten removed 163.8 M / 0.355 s — the base case gets ~9% *worse*. Sub-items (2) and (3) also measured neutral alone and are **not independently shippable**. The file is one slice stale: `b446d316a` landed a partial answer (two env-pure `CallMethodMut` shapes now skip the guard) that it does not record, so the prize — 17% at filing, 10% after the return-merge fix — has shrunk again. **Re-run the `MUTSU_NO_FLATTEN` kill-switch first; it is no longer in the tree and must be re-added as a throwaway.** |
| [yaml-parse-throughput](perf/yaml-parse-throughput.md) | **Round 10's win is CI-CONFIRMED** (independently: the step lands at `8607293b0`, daily min 0.798 → 0.090, `int-arith` flat). Remainder is genuinely flat (allocator ~20%, memcpy 6.7%, `LocalKey::with` 5.7%, SipHash on capture maps 8.1%). **The file needs a hard prune** — rounds 1-9 are ~45 KB of history that round 10 reverses. What must survive is its method lesson, the most transferable finding in the directory: three rounds of counter-diffing missed the multiplier because **every counter compared mutsu against mutsu**; instrumenting the module's own action methods in *both* implementations and diffing found a 40×-over-firing callback in one step. |
| [locals-frame-is-a-pooled-vec-not-a-register-window](perf/locals-frame-is-a-pooled-vec-not-a-register-window.md) | **BLOCKED on an ADR** (the file says so) and structurally unchanged: `locals_pool: Vec<Vec<Value>>` (`runtime/mod.rs:1853`), `take_locals_from_pool` still `pop().unwrap_or_default()` + `clear()` + `resize`. ~5.7% of `bench-fib`'s profile managing a one-element `Vec`. Blast radius is the reusable part: **484 `self.locals` sites across 60 files**, `mem::take` load-bearing in three call paths, `VmCallFrame::saved_locals`, and the JIT knowing the field offset. Same object as `late-august-…` item 1. **Write the `Proposed` ADR before any code.** |
| [digest-ripemd-start-per-block-overhead](perf/digest-ripemd-start-per-block-overhead.md) | **STALE-NUMBERS; re-measure `t/ripemd.t` before touching anything.** The file's own update 6 already retracts the headline — the spawn-shape microbench is 0.19 s mutsu vs 0.41 s raku, so the "per-`start` overhead" premise is closed. The live gap is a `timeout 120` budget against ~148 s. All numbers predate the September call-path sweep and ADR-0058. **Six rounds here have a 3-for-6 record of landing a lever and measuring the gate FLAT.** Its durable note: instruction counts were trustworthy where wall clock on a thermally-throttled box was not. |
| [interpreter-new-is-expensive-and-retains-memory](perf/interpreter-new-is-expensive-and-retains-memory.md) | **STALE-NUMBERS, and partly overtaken.** The file says of `new_regex_scratch` "whether that is on a hot path is unmeasured" — it was measured on 2026-09-06 (207 constructions per parse, ~48% of the run) and largely handled (`Interpreter::new` now skips the built-in registry under `BUILDING_SCRATCH`). Untouched is the full `Interpreter::new()` at ~9.17 ms and **~7.2 KiB retained per construction, linear over 4000**. **Chase the retention — a possible real leak — not the wall clock**, and re-measure in release; the recorded numbers are debug. |
| [closure-literal-creation-cost](perf/closure-literal-creation-cost.md) | Parts A and C done. **Part B is live and untouched**: `vm_register_ops.rs:1063`'s `filtered_flat` still keeps every non-lowercase-initial name in the enclosing scope on **every** closure creation. Part B is the CLAUDE.md flaky-analysis trap by construction (narrowing it means trusting an incomplete free-variable analysis — the `roles-6e.t` precedent), so **cost the cheaper alternative first**: keep the capture semantics and stop rebuilding the map, sharing the system-name portion through the `Env` parent chain. A/B trap: `bench-startup.raku` is too short — the same binary reads 0.0045 s and 0.0086 s in consecutive rounds. |
| [bench-ctor-construction-parity](perf/bench-ctor-construction-parity.md) | **STALE-NUMBERS; merge into `adr0019-g3` and retire.** Its headline ("the only benchmark where mutsu is slower than raku, 1.17-1.35×") is false twice over — the current median is 0.72 and the series cannot resolve anything under ~30%. Both remaining `dispatch_bless` leads are dead (the `class_attrs.iter().position` scan no longer exists and was measured to zero allocations). One residual is live: `format!("__mutsu_sigilless_readonly::{name}")` at `runtime/utils.rs:50`, armed by the shared `closure_meta_keys_possible()` latch. **Keep its round-5 lesson**: never conclude "flat profile" without proving nothing is COMPILED per call — `MUTSU_VM_STATS`'s `add_constant=` must stay near-constant on a steady-state loop, and growth with iteration count is a runtime compile. (That oracle cracked the closure-sequence file open above.) |
| [hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing](perf/hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing.md) | **RECORD; merge into `hash-access-diffuse-regression` and retire.** GC ≈14%, allocation ≈12%, NaN-box decode ≈13%, key hashing+comparison ≈10%. Lead 1 is partly overtaken (`free-variable-reads-stop-cloning-the-current-package` landed 2026-09-07) but the bulk stands: `current_package()` still returns a `String` from an `RwLock` read, and the count is now **236 sites vs 28 `_sym` sites**. Its own instruction holds — get a caller breakdown first, most sites are cold. Leads 2 and 3 both need an ADR. |

Numbers that end up in a document must come from the **bench CI**
(`bench-history.tsv` on `bench-data`), never from a profiling session's own
local runs — and now with the benchmark's **noise class** attached.

---

## Icebox — blocked on a decision, or a pure record

| Ticket | Blocked on / why |
|---|---|
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | Headline defect fixed and re-verified (its six-line repro answers `OUTER` on both). What remains is structural — no `merge_all` equivalent in `call_compiled_closure`, and two disjoint per-instance stores (`closure_env_overrides` vs `closure_captured_state`) — with no measured wrong answer. ADR-0055 slices 3 and 5 *retire* those rather than add a knob. Do **not** re-open its "gap 1 is observably wrong" framing; the file itself retracted it. |
| [containerref-holding-a-hash-is-indistinguishable-from-itemization](deep/containerref-holding-a-hash-is-indistinguishable-from-itemization.md) | **No live divergence** — its repro now dies in both implementations and the destructuring side agrees. A `$`-scalar container holding a hash and a destructuring-slice element container are both spelled `ContainerRef(<Hash>)`. The stopgap holds; `t/hash-itemization-flag.t` is the tripwire. The one actionable delta is cosmetic and independent: mutsu's `X::Hash::Store::OddNumber` renders the offending element tab-separated (`a\t1 b\t2`) where raku prints `Only saw: ${:a(1), :b(2)}`. **Wants an ADR paragraph on the ADR-0039/0036 element-container line, not an opportunistic change.** |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | **Not work — a cross-reference.** All 373 rakudo `X::` subtypes match, six spot probes are byte-identical to raku, and the 2026-08-03 headline repro does not reproduce. It is held open solely for ADR-0029 R5, which is blocked entirely on `vendor-real-test-module`. **Fold R5's one sentence into that file and `git mv` this one to `news/`, or stop re-surveying it.** |
| [lsp-references-needs-a-side-table-not-ast-spans](deep/lsp-references-needs-a-side-table-not-ast-spans.md) | **Measurement before design**, and PLAN §B3 points here rather than at ADR-0065 D6. Its factual claims check out (`ORIGINAL_SOURCE`, `current_line_number`, `is_within_original_source` all where cited; no `references` implementation exists). A thread-local `(offset, name, kind)` table behind the analysis flag touches neither `Expr`'s size nor the bincode cache. The blocker is parser **backtracking** — a `Var` parsed in a failed alternative is a phantom. Build it behind the flag, sweep `modules/`/`vendor/`/`t/`, and let the phantom rate decide; then amend D6/S5b. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | E3/E4 closed; E2b is explicitly demoted to a monitoring signal and does not gate anything. **But the signal has regressed**: `native_call_unmodeled` totals **648 across `t/` alone** today, against ~400 recorded for `t/`+roast in 2026-08-10 — new native methods are landing without rows. Re-run the sweep bucketed by `(owner, name)`; the ADR's own renegotiation says a *new dominant cluster* is still worth a root-cause fix. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Measurement record with reopen conditions. Blocker 3 (parser) is confirmed gone — `repr<Uninstantiable>`, `is native … is ctype<long>` and `use NativeCall` all parse. Blockers 1/2/4 (QAST surface, MoarVM dispatch programs, 61 missing `nqp::` ops) stand, so `NativeCall` remains a justified rung-3 provider under BATTERIES.md. Two counts are stale in the safe direction: **2566** lines across six `nativecall*.rs` files (records 1782), and **36** `use NativeCall` consumers (records 33). |
| [role-body-placeholder-mu-supply](deep/role-body-placeholder-mu-supply.md) | **Its own assessment is "don't"** and it is correct: corpus hits across `roast/`, `modules/`, `vendor/`, `lib/` are **zero**, and rakudo's behaviour is a `VMNull` register artifact (`$^c.^name` → `VMNull`; `.defined` throws). Reproduces exactly. The structural blocker (`run_role_body_for_composition` recompiling each `Plain` op via `run_block_raw`) is ADR-0019 deferred-body machinery — pick it up only if that plumbing is open for another reason. |

---

## Housekeeping notes

- **Closed since the 2026-09-07 regen** — 20 files across 48 merged PRs
  (#7454-#7504): 6 `deep/`, 14 `tickets/`. Highlights: ADR-0058 completed end
  to end (the deferred-map frame bug, `grep`/listop/real-array deferral), the
  whole mixin cluster (application-order rendering, `.^roles`, mixin identity,
  MRO excluding roles), the `$.attr` `.=` RMW origin, free-var resolution in a
  bare block, smartmatch against a bare topic RHS, the itemized hash subscript,
  bracketed callable reductions, native-literal multi dispatch, the custom
  container `STORE` protocol, ratcheted subrule calls going first-only,
  `Template::Nest::Fast` 0/10 → 10/10, rustdoc warnings drained *and gated*,
  and nine call-path perf entries. Details are in `news/2026-09/` (267 files).
- **One ticket was CLOSED and has been retired**:
  `tickets/io-path-methods-test-uses-a-fixed-temp-directory` →
  `news/2026-09/io-path-methods-test-uses-a-unique-temp-directory.md`.
  Its neighbourhood claim is mostly discharged too; only `t/seektype-enum.t:26`
  and `t/io-path-raku-class-roundtrip.t:14` still use fixed `tmp/` names, and
  both write a single file — the lower-risk class the ticket itself called out.
- **Three new bugs found during this regen, each reproduced independently
  twice, now filed as tickets:**
  - [channel-supply-tap-done-callback-never-fires](tickets/channel-supply-tap-done-callback-never-fires.md)
    — `$c.Supply.tap(…, done => {…}); $c.close` never runs `done`
    (`Promise.status` is `Planned` in mutsu, `Kept` in raku), for both bare and
    pointy emit forms. **This is what now blocks a route-5 probe for the Tier S
    gc row** — the old Channel *delivery* blocker is fixed. Two things it
    records as measured-correct, so nobody re-chases them: value delivery is
    ordered and complete, and `await` on a never-kept `Promise` blocks in both
    implementations.
  - [no-pseudostash-type-caller-stash-reports-stash](tickets/no-pseudostash-type-caller-stash-reports-stash.md)
    — mutsu has **no `PseudoStash` type**, and gives *two* different wrong
    answers: `CALLER::`/`CORE::`/`UNIT::` are `Stash` and
    `MY::`/`OUTER::`/`DYNAMIC::`/`LEXICAL::` are a bare `Hash`, where raku says
    `PseudoStash` for all seven (`OUR::` is correctly `Stash`). Measured:
    `PseudoStash.^mro` is `(PseudoStash, Map, Cool, Any, Mu)` — it is **not** a
    `Stash` subclass. This is an unnamed prerequisite of
    `deep/p5tie-stash-bind-key-protocol`, and the cheaper testable half of it.
  - [nativecall-type-table-shadows-a-user-class-of-the-same-name](tickets/nativecall-type-table-shadows-a-user-class-of-the-same-name.md)
    — `class void { }; say void.^name` → `NativeCall::Types::void` (raku
    `void`) with no `use NativeCall` anywhere; same for `long`, `ulong`,
    `size_t`. `int8` is correct, which is the discriminator: the failing names
    are the ones supplied only by the NativeCall table. Subs and variables of
    the same name are unaffected. This is the one live bug
    `deep/nativecall-cannot-be-vendored` recorded and declined to file.
  Two further candidates were left unfiled and are recorded in their parent
  files instead: `.returns(:qqzz9)` leaking `&<composed-method:returns>`, and
  the RakuAST `with` refusal dumping a raw Rust `Debug` struct into a
  user-facing message.
- **`docs/doc-diff-backlog.md` was re-swept and rewritten on 2026-09-07b — the
  feeder is NOT drained, it was mis-bucketed.** All 25 ticket links in its
  Ticketed section were dead (every ticket had been fixed), so the section was
  refilled from a fresh full-corpus sweep at `dccfd1737`: high-signal
  108 → **87** (mismatch 74 → 59, crash 34 → 28, `match` 2376 → 2402).
  **The important finding is not the counts.** The harness's `raku-drift`
  bucket is only reachable *after* mutsu is found to differ from raku, yet the
  backlog told readers it was "version skew, not mutsu bugs — lowest priority".
  Classifying all 114: **67 (59%) are confirmed real mutsu divergences**
  against 5 (4%) that the bucket name describes — a larger pool of real work
  than the 87 the survey table ranks, and 61 of the 114 sit in files the table
  cannot show at all. 37% of the bucket is pure noise from doc lines that froze
  an address or an iteration order raku itself cannot reproduce twice, and
  **nine real bugs were hiding under that noise**. Tracked in
  [doc-diff-harness-has-no-output-cap-or-nondeterminism-gate](tickets/doc-diff-harness-has-no-output-cap-or-nondeterminism-gate.md);
  the fix is one policy line (run the oracle twice, drop non-reproducible
  blocks), not a pattern list.
- **`todo/` files whose own root-cause or status section is wrong** — still the
  project's most common failure mode, and this cycle it took a new and more
  dangerous form: **a repro that passes while its bug is still live.** Two of
  the five Tier S rows are in that shape (`is-type-capture-…` needs its blocks
  reversed; the gc row needs `[i] =` rather than `.push`). Also this cycle:
  `perf/interpreter-call-path-in-hot-loops` (third consecutive framing dead —
  `interpreter_fallbacks` is 0.0%, not 83.3%), `perf/bench-ctor-construction-
  parity` (its "slowest benchmark" headline and both remaining leads),
  `perf/closure-sequence-…` (blames `max :by`, which does nothing),
  `deep/native-method-accepted-named-declarations` (two section-3 rows now
  agree, one is worse than recorded), `deep/template-engines-…`
  (`Template::Classic`'s diagnosis does not reproduce reduced),
  `deep/config-toml-…` (two residues need dist context, not a reduced repro),
  `tickets/mro-roles-adverb-…` (its `.^roles` premise is backwards),
  `tickets/array-subclass-assignment-…` (its control is the bigger bug), and
  `deep/slurpy-hash-named-arg-…` (its `.VAR` discriminator now passes
  coincidentally). ADR-0041 §6 remains the model for recording this: keep the
  document, add a section naming which premises were measured false and why.
- **Three ADR headers are behind their own bodies** — ADR-0011 (progress note
  dated 2026-08-02, and its `rakuast-remaining` inventory undercounts 113 files
  as 93), ADR-0065 ("S0 and S1 shipped" vs a phasing table marking S5a done),
  and ADR-0070 ("slice 1 implemented" vs §Implementation recording slice 2).
  ADR-0033's status line is likewise behind — all four of its phases shipped.
- **A wall-clock gate recorded without its calibration is not portable**, and
  this cycle adds the sharper version: **the raku ratio does not save you
  either** on allocation-heavy OO benchmarks (see the Perf section). Record the
  benchmark name, the ratio, *and* the noise class.
- Verification for this regen was run ad hoc from `tmp/` (gitignored); each
  ticket's own repro block regenerates it.
