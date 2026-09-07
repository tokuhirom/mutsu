# TRIAGE — prioritized snapshot of todo/ (2026-09-07)

A ranked index of every open finding under `todo/tickets/`, `todo/deep/` and
`todo/perf/`, so a session can pick the next unit of work without re-reading
all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

## What changed since the 2026-09-06 regen (`884366c95`)

Surveyed at `e3f2ae5d8`, then folded forward to `f62654e3d`: **67 files** —
30 `deep/`, 22 `tickets/`, 15 `perf/` (was 75: 39/20/16).

**54 PRs merged in one day** (#7398-#7453). That is the largest single-day
turnover this file has recorded, and it invalidated most of the previous
snapshot. Four of them (#7450-#7453) landed *during this survey* and closed
four rows that were already written; they are folded in below, and the two
files they filed in exchange are ranked.

- **35 `todo/` files closed** — 17 `deep/`, 16 `tickets/`, 2 `perf/`. Both of
  the previous snapshot's Tier S rows are gone from that tier: the baggy
  bigint-weight panic is fixed
  (`news/2026-09/baggy-operators-carry-bigint-weights.md`), and the
  cross-thread row moved from "an ADR with no code" to "ADR-0068 §4 steps 1-2
  implemented". Details for every closure are in `news/2026-09/` (219 files).
- **Seven ADRs were written or advanced in a day.** ADR-0068 went `Proposed` →
  **`Accepted`, steps 1-2 shipped**; ADR-0058 step 2 shipped; ADR-0067 finished
  its last producer; and four new ones landed — **ADR-0070** (native methods
  declare accepted nameds), **ADR-0071** (native operators are dispatch
  candidates), **ADR-0072** (a resumable exception runs its handler at the throw
  point), **ADR-0073** (regex atom candidates are demand-driven, slices 1+3
  shipped), **ADR-0074** (a channel-backed Supply broadcasts to its taps).
- **`tickets/` refilled again, from three sources this time**: the
  `docs/doc-diff-backlog.md` re-sweep (as before), the **neighbourhood lists of
  the day's own fixes** (nine of the twelve new tickets were found while fixing
  something adjacent — the mixin cluster, the regex cluster, the reduce
  cluster), and ADR-0073's measurement sweep.
- **The previous regen's perf headline was a misread, and is corrected below.**
  It reported "`bench-fib` 0.84x → 0.37x". Those are two different benchmarks:
  at `87e910a62`, `bench-fib` was **0.82** and `fib` was **0.37**. `bench-fib`
  has not moved (0.86 today). See the perf section.
- **The `Test`-provider retirement moved again**, 29.5s → **27.4s** against a
  hard 30s gate on the reference box, and its file now names three specific
  perf rows as what is left. Re-measured here at **8.4x rakudo** — see that
  row for why the ratio, not the second-count, is the number to carry.

### What was re-verified for this regen

Every file was read, and 48 runnable repros were run against `raku` v2026.07 on
a fresh `target/debug/mutsu` at `e3f2ae5d8`. Findings:

- **Every ticket reproduced as written** at the survey commit, including all
  twelve filed since the last regen. Three of them (`eval-returns-enum-...`,
  `triangle-reduce-over-a-lazy-pipe-...`, `unreached-end-phaser-lexicals-...`)
  were then fixed by #7450/#7451/#7453 while this regen was being written —
  the repros above are the pre-fix measurement and the rows are gone.
- Three `deep/` rows have moved and are called out inline
  (`native-method-accepted-named-declarations`,
  `containerref-holding-a-hash-...`, `subscript-argument-container-producer`'s
  headline).
- `tickets/subscript-argument-container-producer`'s **headline is closed** by
  ADR-0067's `IndexArgRef` — `S.new.take(@a[0])` writes through now — and the
  file has been correctly rewritten down to three residue rows. Verified.
- `deep/native-method-accepted-named-declarations`: all three probes the
  previous regen listed as wrong (`chop(:zzz)`, `polymod(3,:zzz)`,
  `fmt("%d",:zzz)`) now answer **byte-identically to raku**. ADR-0070 slice 2
  drained them; what is left in that file is `subst`/`trans` hardening, not a
  live bug.
- `deep/containerref-holding-a-hash-...`: its repro now dies in **both**
  implementations (mutsu's message text differs). It is a representation
  record with a stopgap in place, not a live divergence — moved to the Icebox.
- `rustdoc-doc-link-warnings` drifted again: 210 (filed) → ~248 (last regen) →
  **251** measured today.

**Standing caveat.** A tier is a routing hint. This is one run on one box;
CLAUDE.md's rule still applies — re-verify a ticket's repro on your own build
before acting on it, and read the *tail* of a ticket file, where successive
"Re-verified" sections accumulate and can themselves rot.

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

## Tier S — Soundness (crashes, silent data loss)

| Ticket | Breadth | Effort | Verified 2026-09-07 |
|---|---|---|---|
| [gc-contents-mut-cross-thread-aliased-writes](deep/gc-contents-mut-cross-thread-aliased-writes.md) | ADR-0068 §4 **step 3**: the routes that are still neither name-keyed nor cell-guarded | XL | **Much smaller than it was, and no longer un-owned.** ADR-0068 is `Accepted` and its steps 1-2 are shipped: a cell-keyed stripe lock (`src/value/container_lock.rs`) on element stores *and* the `with_deref`/`into_deref` read chokepoints, plus an allowlisted mutating-method funnel. Routes 1/3/4 went 17/96, 96/96 and 64/64 failures to **0/240 at 24-way**; the file's own named-sub probe answers `1000` on this build. What is open: attribute/twigil names, masked re-declarations, containers never in a spawning frame's env, and non-name-keyed writes. **The evidence says three funnels, not 149 sites** — probe a new route onto one of them before assuming a fourth. Two loose ends stay explicitly open: route 5 (`Channel.Supply` tap captures) behind a separate delivery bug, and the `roast/S17-procasync/stress.t` SIGSEGV, still unexplained. |
| [deferred-map-callback-runs-in-the-consuming-frames-env](deep/deferred-map-callback-runs-in-the-consuming-frames-env.md) | every deferred `.map` callback that reads a routine parameter | L | **NEW, and confirmed as non-termination**: a recursive `g(@sizes)` whose body is `[1].map({ g(@sizes[1..*]) })` reads `@sizes` from the frame active at the *pull*, not the frame that bound it, so depth 3 sees depth 1's `(2, 1)` and the recursion never reaches its base case (measured: mutsu runs away, raku terminates at depth 3). The parameter is bound correctly at every call — the `note` proves it — so this is a read landing in the wrong frame. It aborted the whitelisted `roast/integration/99problems-21-to-30.t` with a **stack overflow** when ADR-0058 step 3 landed, which is why step 3 was reverted. `create_lazy_map_list` gets this right with one `self.env.clone()`; `SeqSource::MapGrep` carries no equivalent, and putting one there moves an `Env` clone onto the now-hot path. **Blocks ADR-0058 steps 3 and 4.** |
| [decimal-literal-with-big-integer-part-loses-it](tickets/decimal-literal-with-big-integer-part-loses-it.md) | every decimal literal whose integer part exceeds `i64::MAX` | **S** | **Confirmed**: `1000000000000000000000000000000.5` evaluates to **`0.5`**, silently. Same shape as the baggy bigint weight row this tier just closed — a wrong *value*, not an imprecision, with no warning. The threshold is exactly `i64::MAX`. Root cause is one `unwrap_or(0)` in `src/parser/primary/number.rs`: it swallows the `parse::<i64>()` failure that was the only signal selecting the (already correct) BigInt arm. A routing fix, not new arithmetic — the smallest Tier S row this file has ever carried. |

---

## `todo/tickets/` — all 22

All twenty-two reproduce exactly as written. One is ranked above (Tier S) and
one is in the Icebox; the rest are here.

### Broad correctness (7)

| Ticket | Tier | Note (verified 2026-09-07) |
|---|---|---|
| [smartmatch-against-the-topic-returns-bool-not-match](tickets/smartmatch-against-the-topic-returns-bool-not-match.md) | B1, **M** | **Confirmed**: `for (/a/,) { say ("ab" ~~ $_).raku }` gives `Bool::True`; raku a full `Match`. `exec_smart_match_expr_op` sets `$_` to the LHS *before* running the RHS bytecode range, so an RHS that reads `$_` sees the wrong thing. `EXPR ~~ $_` inside `for`/`given` is a very common idiom. Needs a design split between `~~`-as-topicalizer and a plain-expression RHS, not a patch. |
| [qualified-call-falls-back-to-bare-global-routine](tickets/qualified-call-falls-back-to-bare-global-routine.md) | B1, **M** | **NEW, confirmed, and a silent wrong-routine dispatch**: `sub zzz($n) {...}; NoSuchPkg::zzz(1)` answers `2`; raku dies `Could not find symbol '&zzz' in 'GLOBAL::NoSuchPkg'`. `fn_keys_for_base` reduces `Pkg::name` to the short name, so the fallback gather can return a routine from a completely different package — a typo'd or stale qualifier silently calls the caller's own same-named sub. It also makes `t/module-private-sub-does-not-leak.t`'s intended assertion untestable, so it **gates** `deep/module-toplevel-private-sub-leak-cleanup`. Expect fallout: the qualified path is also how a `unit module`'s `our sub` is reached. |
| [array-subclass-iterator-override-ignored](tickets/array-subclass-iterator-override-ignored.md) | B1, **M** | **Confirmed**: `class SortedArray is Array {...}; .say for $thing` prints the whole array as one item; raku iterates the override's four. A user `iterator`/`list` override on an `is Array` subclass is not consulted, and `for` decides iterability from its own shape analysis. The same `__mutsu_array_storage` guard is named in `docs/doc-diff-backlog.md`'s deferred lazy-list cluster. |
| [itemized-hash-subscript-is-a-slice-not-one-key](tickets/itemized-hash-subscript-is-a-slice-not-one-key.md) | B1, **M** | **NEW, confirmed, and its write face is silent misdirection**: `my $s = $(1,2); %c{$s} = "x"` writes key `"2"`; raku writes `"1 2"`. mutsu flattens the itemized value into a two-element slice and assigns each in turn, so the last one wins. The read side is wrong the same way, and on a `%c{List:D}` hash it surfaces as `expected List:D but got Int (2)` — the `Int (2)` being the tell. Read and write must move together. Split from the closed `object-hash-key-lost-when-pair-value-is-a-container`. |
| [native-int-candidate-loses-to-int-for-a-literal](tickets/native-int-candidate-loses-to-int-for-a-literal.md) | B1, **M** | **Confirmed**: with `multi d(int $x)` and `multi d(Int $x)`, `d(5)` picks `boxed`; raku `native`. A literal arrives with no declared-type source, so `candidate_type_distance` ranks `Int` closer. The file's own fix sketch (mark literal native types in the arg-sources constant) carries a **measure-first warning**: that constant is elided entirely when every entry is `NIL`, so marking literals would materialize it at a large fraction of all call sites. |
| [typed-container-capture-still-loses-to-a-same-named-caller-array](tickets/typed-container-capture-still-loses-to-a-same-named-caller-array.md) | B1, **L** | **Confirmed**: a closure over `my Int @a` reads `1` where raku reads `3` once a callee declares its own `@a`. The **last hole** in the capture-cell dichotomy (ADR-0055 slice 1b). The file records two *measured* regressions from the naive widening — lifting the typed refusal broke `my %h is BagHash = ...` initialisation and dropped 24 subtests across `baghash.t`/`mixhash.t` — so read it before trying the obvious fix. Its `ApplyVarTrait` half pairs with the inline-trait ticket below. |
| [global-match-scan-enumerates-every-end-at-every-start](tickets/global-match-scan-enumerates-every-end-at-every-start.md) | B1, **M** | **NEW, confirmed**: a code block inside `m:g/…/` runs **12** times against raku's 4, and inside `.subst` twice against raku's 1. `regex_match_all_*` brute-forces `0..=len` start positions and collects every end at each; raku finds one match, commits, and resumes after it. Broad because it is on every `:g` scan, every `subst`, `comb` and `split` — so the first step is enumerating which callers actually want overlapping ends. ADR-0073's sweep split this out deliberately: slice 1 already halved the `subst` row. |

### Narrow correctness, diagnostics, permissiveness (13)

| Ticket | Tier | Note (verified 2026-09-07) |
|---|---|---|
| [an-our-declared-in-a-never-run-block-is-not-installed](tickets/an-our-declared-in-a-never-run-block-is-not-installed.md) | N, **M** | **NEW (#7453's exchange)**: `if False { our $o = 4 }; say OUR::<$o>.^name` finds no symbol; raku answers `Any`. rakudo installs a package symbol when the compunit is *compiled*, so the slot exists undefined even though the assignment never ran. It is the `our` half of the END-phaser lexical fix that just landed for `my`/`state` — a declaration whose **symbol** is compile-time and whose **value** is run-time. The `EndWalker` is the shape to copy, but the walk must track `Compiler::qualify_our_variable_name`'s pseudo-package resolution and must not disturb the path where the branch *does* run. Pairs with `state-and-our-typed-declaration-hoist`'s second row — both are `our`-declaration-time gaps. |
| [array-and-hash-which-collides-on-a-reused-address](tickets/array-and-hash-which-collides-on-a-reused-address.md) | N, **M** | **Confirmed, and re-diagnosed since the last regen** — read the file, not the title. `[1,2].WHICH eq [3,4,5].WHICH` is `True`, which rules out the previously recorded "content hash". `.WHICH` is already pointer-based, exactly as raku's is; the two *temporaries* simply land on the same recycled allocator block. Two containers held in variables compare correctly. The fix is a lazily minted per-object id in `ArrayData`/`HashData`, which is a layout change on the two hottest container types and wants an alloc measurement. `===`, `eqv` and object-hash keying are all already correct (`values_identical` uses `Gc::ptr_eq` on live values), which bounds the damage. |
| [inline-container-trait-declaration-in-an-expression-is-a-plain-hash](tickets/inline-container-trait-declaration-in-an-expression-is-a-plain-hash.md) | N, **M** | **NEW, confirmed**: `(my %q is SetHash).^name` is `Hash`; raku `SetHash`. Narrowed in the file to the *expression position*, not the trait — declared as its own statement and read afterwards everything agrees. `ApplyVarTrait` runs after the declaration pushes its value, so the declaration's result is the un-coerced `Hash`. Same op as the typed-container-capture ticket above; a fix should check both. |
| [mixin-picks-the-wrong-role-group-candidate](tickets/mixin-picks-the-wrong-role-group-candidate.md) | N, **M** | **NEW, confirmed**: with `role Z {...}` and `role Z[::T] {...}`, `(1 but Z[Str]).a` dies `No such method 'a'`; raku `(Str)`. The registry already models role groups — `resolve_role_candidate`/`role_candidates` is what the class-header path uses, and `class C does Z[Str]` is measured correct. Only `compose_role_on_value`'s `registry().roles.get(role_name)` skips them. Route the mixin path through the existing selector; do not write a second one. |
| [mixin-roles-introspection-omits-the-mixed-in-role](tickets/mixin-roles-introspection-omits-the-mixed-in-role.md) | N, **S** | **NEW, confirmed**: `(1 but A).^roles` lists `Real,Numeric`; raku `A,Real,Numeric`. `.^name`, `.does` and `~~` are all already right, so only the `^roles` arm answers for the base type. `role_mixin_suffix_excluding` already enumerates exactly the markers needed. Note raku's order: the mixed-in role comes first. |
| [multi-role-mixin-name-joins-with-a-comma](tickets/multi-role-mixin-name-joins-with-a-comma.md) | N, **M** | **NEW, confirmed**: `((1 but A) but B).^name` is `Int+{A,B}`; raku `Int+{A}+{B}`. Two things differ, not one — the bracketing *and* the order (mutsu sorts alphabetically for determinism; raku shows application order). mutsu already stamps `__mutsu_role_seq__` with a monotonic id, so sorting by that gives both. The file also measures a second consistent face: `(1 but A) but B` and `(1 but B) but A` have `=:=`-identical `.WHAT` in mutsu and distinct ones in raku — decide both together. `.^name` is load-bearing in every `X::` message, so grep for `+{`-with-a-comma pins first. |
| [two-identically-mixed-values-are-not-identical](tickets/two-identically-mixed-values-are-not-identical.md) | N, **M** | **NEW, confirmed**: `(1 but A) === (1 but A)` is `False`; raku `True`. Narrowed: `.WHAT =:= .WHAT` is `True` in both, so ADR-0060's composition key is right and only `values_identical`'s `Mixin` arm disagrees with it — most likely by comparing the per-application `__mutsu_role_seq__` stamp. The file's neighbourhood list is unusually good: different *initialisers* must NOT make them differ (`(1 but R(2)) === (1 but R(3))` is `True` in raku, measured), while a reference-type base must stay `False`. |
| [mro-includes-composed-roles](tickets/mro-includes-composed-roles.md) | N, **L** | **Confirmed**: `K.^mro` lists `K,R2,Any,Mu`; raku `K,Any,Mu`. Introspection output conflated with dispatch order. **L, not S, deliberately** — MRO widening has caused an unrelated dispatch-table regression here before (`trap-mro-widening-...`), so separate the two consumers rather than editing one list. Must not start listing mixins as a side effect of the `.^roles` ticket above. |
| [noncapturing-group-block-under-counted-separator](tickets/noncapturing-group-block-under-counted-separator.md) | N, **M** | **NEW, confirmed, and the MATCH is wrong, not just the count**: `"a,b,c" ~~ / [ \w+ {B} ] ** 1..3 % ',' /` matches `｢a｣`; raku `｢a,b,c｣`. Needs all three of a non-capturing group, a code block inside it, and a counted `** N..M` separated quantifier — swap any one and it is correct, which is a very tight discriminator. Pre-dates ADR-0073 (the eager producer gives the same wrong match). Pinned `todo` as F6/F6b in `t/regex-lazy-candidate-enumeration.t`. |
| [reduce-with-a-bracketed-callable-op-does-not-parse](tickets/reduce-with-a-bracketed-callable-op-does-not-parse.md) | N, **M** | **NEW, confirmed**: `[\[&f]] 1, 2, 3` and `[[&f]] 1, 2, 3` are both `Confused.`; raku `(1 5 11)` / `11`. Purely a **syntax** gap — reducing with a user-defined `infix:<>` already works in both the fold and scan forms, and `reduction_callable_for_op`/`reduction_step_with_args` already accept a `callable`. Note `[&f] 1..5` (no inner brackets) is a parse error in raku too; support the bracketed spelling only. |
| [subscript-argument-container-producer](tickets/subscript-argument-container-producer.md) | N, **M** | **Headline CLOSED 2026-09-07** and re-verified here — `S.new.take(@a[0])`, `$r(@a[0])`, `&g(@a[0])`, a pointy and a bare-block topic all bind the element now (`OpCode::IndexArgRef`). Three residue rows survive: `$obj.^lookup('m')($obj, $c.v)` (a `Method` invoked as a code value counts its invocant as positional 0); `sub g(:$y is rw)` accepted where raku refuses the declaration; and an **out-of-range** subscript argument on the three nameless-callee shapes, where the two producers' shared "an existing element" contract has to stop being shared. |
| [immutable-list-element-write-is-silently-dropped](tickets/immutable-list-element-write-is-silently-dropped.md) | N, **L** | **Confirmed**: `$l[0].mut` on `my $l = (1,2)` with a `\S:` raw invocant silently succeeds and changes nothing; raku dies `Cannot modify an immutable Int (1)`. The write is correctly dropped — only the *diagnostic* is missing, because the binder does not consult readonly-ness for a raw invocant. Every ordinary store to such an element is already refused (A2-A4 measured), so this is a method-call-path gap; ADR-0067 rows L4/L5/M1/M2. |
| [state-and-our-typed-declaration-hoist](tickets/state-and-our-typed-declaration-hoist.md) | N, **S** | **Confirmed, two independent rows.** `state Int $x` is not in effect before its declaration statement (raku raises `X::TypeCheck::Assignment` through an `EVAL`, mutsu answers `NO-DIE`); and `our Int $x` compiles and enforces at *runtime*, where raku refuses at compile time (`Cannot put a type constraint on an 'our'-scoped variable`). Residue of `d61e6c534`; the file explains exactly why the hoist pre-pass excludes `state` (the `SetVarType` op also seeds, which would reset a live `state`). |

### Icebox tickets (1)

| Ticket | Why here |
|---|---|
| [rustdoc-doc-link-warnings](tickets/rustdoc-doc-link-warnings.md) | Pure lint debt with no functional repro and no CI gate. **Its headline number has now drifted twice**: 210 recorded, ~248 at the last regen, **251** measured today (`cargo doc --no-deps --document-private-items`). The per-category breakdown is stale; the taxonomy is probably still right. Fix the count when you fix the warnings, or add the gate first so it stops drifting. |

---

## How to work `todo/deep/` — by ADR cluster

**Do not run `deep/` oldest-first** (filing order is an accident of which
campaign ran last, and `ls -tr` mtimes are corrupted by worktrees). Work it by
ADR cluster: most deep findings wait on a *slice of an ADR that already
exists*, and one landed slice closes several rows. Every `Status` line below
was read on 2026-09-07.

| ADR | Status | Rows it would close |
|---|---|---|
| [ADR-0068](../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md) cross-thread container writes | **`Accepted`; §4 steps 1-2 IMPLEMENTED, step 3 started.** §7 records three premises of the original design that measurement contradicted — read it first. §9 retired the `thread_escaping_captures` exclusion, so a `start` block that names a container lexically now takes the guarded cell path. | The remaining Tier S row. Still the only ADR that owns one. |
| [ADR-0058](../docs/adr/0058-map-grep-produce-a-deferred-seq.md) map/grep produce a deferred Seq | **`Accepted`; step 2 shipped 2026-09-07**, steps 3-4 open. Step 3a was implemented, measured green on `make test`, and **reverted** when roast found the frame bug below. §8/§9 record what step 0 measured and which premises did not survive. | `deep/deferred-map-callback-...` is the **blocker** for steps 3 and 4, not a consequence of them. Step 2 already closed 9 of `residual-try-cell-...`'s 11 rows. |
| [ADR-0073](../docs/adr/0073-regex-atom-candidates-are-demand-driven.md) regex atom candidates on demand | **`Proposed`; slices 1 and 3 implemented 2026-09-07**, slice 2 (the `<subrule>` boundary) open. | `deep/ordered-alternation-...` **is** slice 2, and now carries a 4-row measured table (E1/E2/E3/E6). **E3 is the cheap half** — a ratcheted caller cannot backtrack into the subrule, so `first_only` fixes it outright and cuts waste on every bundled grammar. |
| [ADR-0067](../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md) a routine hands back its container | **Accepted; every slice implemented**, including the subscript-receiver and nameless-callee argument producers on 2026-09-06/07. | Its residue is now three rows inside one ticket (above) plus `tickets/immutable-list-element-write-...`. The argument-side gap the previous regen listed is closed. |
| [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) closure free vars bind their own | Slices 1 + 1b landed (1b's parameter carve-out retired the same day, §7.7); slices 2-5 open. §7.6 records that the ADR was wrong about there being two closure-env merges — there are three. | `tickets/typed-container-capture-...` (the last measured hole); `deep/free-var-lexical-resolution-inside-a-bare-block`; `deep/call-compiled-closure-...` (structural only — its headline repro passes). |
| [ADR-0039](../docs/adr/0039-container-lexicals-resolve-lexically.md) container lexicals resolve lexically | Slice 1 landed 2026-08-20; §8.2 closed; **slice 2's stated blocker is measured gone**. | `deep/adr0039-slice2-container-reads-compile-to-a-slot` — a brand-new file that exists precisely because the §10.3 blocker (a container mutated from a nested frame propagating by NAME only) was closed by `da8e94252` and re-verified by building at `da8e94252^`. |
| [ADR-0070](../docs/adr/0070-native-methods-declare-the-named-arguments-they-accept.md) native methods declare accepted nameds | Slice 1 built the mechanism; **slice 2 drained the measured residue** 2026-09-07 (78 → **18** of 1 422 probes; raku's own baseline is 23, and only 9 of the 18 are mutsu-specific). | `deep/native-method-accepted-named-declarations`, which is now **hardening, not a live bug** — its remaining item is `subst`/`trans`, whose adverb sets are read out of a slurpy and must be established by hand. |
| [ADR-0047](../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md) type identity | Partially adopted — P1/P2 landed; **P3 and P4 not started** | `subtest-compiled-dispatch-async-middleware-regression` (P4 is the prerequisite for re-landing #6499, *not* itself the fix). |
| [ADR-0053](../docs/adr/0053-do-whenever-produces-a-tap-on-the-stack.md) `do whenever` produces a Tap | `Proposed`, "**partially implemented** — see §8". Slice 1 landed 2026-09-07: `do whenever` in expression position answers a real `Tap`, and every delivery shape agrees with rakudo **as long as nothing closes the tap**. | `whenever-expression-position-needs-real-design`, now down to one row: with `$tap.close`, mutsu drops both emissions. |
| [ADR-0065](../docs/adr/0065-language-server-targets-ai-agents.md) LSP targets AI agents | Accepted; header still says "S0 and S1 shipped" while its own phasing table marks S5a done. | `lsp-references-needs-a-side-table-not-ast-spans` (amends D6/S5b; blocked on a *measurement*, not a design). |
| [ADR-0029](../docs/adr/0029-exception-class-role-membership.md) exception class role membership | Slices 1-3 + R1-R4 landed; **R5 blocked on `vendor-real-test-module`** | `exception-class-hierarchy-is-mostly-unregistered` — title badly stale, all 373 rakudo `X::` subtypes match. Closed-pending-an-unrelated-dependency. |
| [ADR-0048](../docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md) placeholder scope | Accepted; P1-P4 landed, P5's scope half landed, value half deferred | `role-body-placeholder-mu-supply` — and the file argues against doing it: corpus hits are zero and rakudo's behaviour there is an artifact. |
| [ADR-0059](../docs/adr/0059-is-rw-routines-return-a-container.md) / [ADR-0051](../docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md) / [ADR-0021](../docs/adr/0021-argument-namedness-is-a-call-site-property.md) / [ADR-0025](../docs/adr/0025-captured-scalar-cells-value-kind-blind.md) | Slice 3 / P2+P5 / P5 / slice 3 open respectively | Nothing in `todo/` names them. Open slices with no failing repro — do not resource them ahead of a Tier S row. |
| [ADR-0050](../docs/adr/0050-block-routine-ness-is-a-definition-site-property.md) / [ADR-0043](../docs/adr/0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md) | Both `Proposed`, implementation not started (ADR-0043's Decision 1 is probe-verified and ready) | Nothing in `todo/` — designs waiting for a consumer. |

### Recommended next campaigns

1. **ADR-0058's frame bug — the highest-value single fix in the file.**
   `deep/deferred-map-callback-runs-in-the-consuming-frames-env` is Tier S on
   its own (a read landing in the wrong frame, with a stack-overflow face), and
   it is simultaneously the **only** thing blocking ADR-0058 steps 3 and 4,
   which retire `create_lazy_map_list` — one of the two deferral mechanisms
   that currently disagree about the most basic property of a deferred
   callback. The obvious fix (an `Env` snapshot on `SeqSource::MapGrep`) is
   already what the *other* mechanism does, but ADR-0058 made every `.map`
   deferred, so it moves an `Env` clone onto the hot path. Measure
   `env_deep_copies` on debug before adopting it, and cost the two cheaper
   shapes the file names (capture only `free_var_syms`; or make parameter
   capture work at deferral time, which is the ADR-0055 answer).
2. **ADR-0068 step 3 — still the only Tier S *cluster*, and now much cheaper
   than it looks.** Steps 1-2 did the hard part: three funnels are guarded and
   the four measured routes are 0/240 at 24-way. What is left is classifying the
   remaining routes onto those three funnels, each with its own oracle-classified
   probe. **Do not start with a 149-site sweep** — the ADR rejects that on the
   record, and the evidence says three funnels, not 149 sites. Two named loose
   ends are genuinely open work: route 5 (behind a Channel-supply delivery bug
   that must be fixed first) and the unexplained `S17-procasync/stress.t`
   SIGSEGV.
3. **Finish the `Test`-provider retirement (a PLAN §1 goal, not perf polish).**
   `deep/vendor-real-test-module` is at **8.4x rakudo** on `write-int.t`
   (re-measured for this regen: 9.61s vendored / 1.92s native / 1.15s rakudo on
   an idle box ~3x faster than the file's; 27.4s against a hard 30s gate there,
   with single samples reaching 30.4s) — and it
   named three perf rows as its remaining work, and **two of the three are now
   measured out of it**: `defaulted-param-forfeits-the-light-call-path` closed
   during this regen but did *not* move `write-int.t` (`ok` is a multi;
   `proclaim` carries `is copy` + a coercion, so neither becomes eligible), and
   `listop-call-bypasses-every-compiled-call-cache`'s carrier arm measured at
   1.2%. That leaves `perf/method-dispatch-flattens-the-env-on-every-call`,
   whose obvious fix is itself **measured neutral-to-negative** — read its
   2026-09-06b update before touching it. **So this campaign no longer has a
   named next step: its first task is to re-profile the file and find one**,
   which is a better use of a session than any of the three stale rows. The
   correctness sweep is clean (`t/` regressions 0), so perf is the only gate.
4. **The mixin cluster — five tickets, three of them one arm apiece.**
   `mixin-roles-introspection-omits-the-mixed-in-role`,
   `multi-role-mixin-name-joins-with-a-comma`,
   `two-identically-mixed-values-are-not-identical`,
   `mixin-picks-the-wrong-role-group-candidate` and (adjacently)
   `mro-includes-composed-roles` all landed in one day from the same fix's
   neighbourhood sweep, and four of them read or write the *same* three
   structures: `role_mixin_suffix_excluding`, the `__mutsu_role_seq__` stamp,
   and ADR-0060's composition key. Two of them explicitly warn each other off
   (`.^roles` must not make `.^mro` start listing mixins). Worth doing as one
   deliberate pass with a shared pin file rather than four independent PRs
   that each re-derive the same map.
5. **ADR-0039 slice 2 — the enabler whose blocker is measured gone.**
   `deep/adr0039-slice2-container-reads-compile-to-a-slot` exists because the
   §10.3 withdrawal reason no longer reproduces (verified by building at
   `da8e94252^` and running six probe shapes). Flipping `@`/`%` reads to
   `GetLocal(slot)` is what makes a whole class of "the slot and `env` disagree"
   defects *visible instead of hidden*. It is the third attempt, so the file's
   own instruction stands: **re-measure the four store-side defects first** —
   they were measured before `da8e94252` and some may have gone the same way
   the blocker did.

**One measured process exception, kept from previous regens:** when a change
alters a *universal property of values* ("what is in every container"), run the
full local `make roast` before pushing (ADR-0040 slice 2 needed 17
counter-current fixes, 9 found only by roast). Campaign 5 above is exactly that
shape and its own acceptance section demands it. Ordinary parser/operator/
dispatch fixes still delegate to CI.

**Methods worth copying.** Five, all earned or re-earned this cycle:

- **A neighbourhood sweep after a fix is the highest-yield ticket source
  there is.** Nine of the twelve new tickets came from measuring the shapes
  *next to* something that was being fixed, not from a corpus scan. Three of
  them (the mixin cluster) came out of a single name collision inside a test
  that was being written for an unrelated fix. Budget the sweep as part of the
  fix, and write the neighbourhood list into the ticket — the four mixin
  tickets' neighbourhood sections are what make them cheap now.
- **Prove the *shape* of a divergence with a control table before writing a
  root cause.** `noncapturing-group-block-under-counted-separator` needs all
  three of a non-capturing group, a code block and a counted separated
  quantifier; three neighbouring shapes are correct. That table is worth more
  than the file's own (explicitly unverified) guess at the mechanism, and it
  survives the guess being wrong.
- **ADR-0068's stress harness — CPU *oversubscription* is the ingredient, not
  concurrency.** At 8-way on 12 cores a racing workload was clean; at **24-way
  on 12 cores** the same binary failed within seconds. Every "clean"
  measurement taken below that threshold means nothing. Negative results kept
  on the record: `memcheck` serializes threads onto one core and finds nothing;
  helgrind was a dead end.
- **The two-breakpoint gdb oracle.** Break on the unsynchronized site and on
  the synchronized lane; `already hit N times` on the first with nothing on the
  second means the workload is *exposed*. Deterministic, one debug run, and it
  settled every route in minutes where the stress harness needs hundreds of
  runs to say the same thing probabilistically. This is also how the
  `.list.map` lost-writeback row was located: breakpoint counts on three
  candidate paths, per probe.
- **`alloc_scope!` sub-scoping beats a guess, and disproved one this cycle.**
  `bless:named-args`'s recorded cause (an O(attrs × args) scan wanting an
  index) was wrong: sub-scoping the loop attributed **all 11** allocations to
  the `%`-sigil coercions and **zero** to the scan. Indexing would have bought
  nothing. Sub-scope the region before assuming where inside it the cost is.

---

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness

| Ticket | Effort | Why here |
|---|---|---|
| [free-var-lexical-resolution-inside-a-bare-block](deep/free-var-lexical-resolution-inside-a-bare-block.md) | L | **Residue** of the closed `free-var-read-in-callee-resolves-through-dynamic-caller-chain`, whose file-scope half landed 2026-09-07 (`news/2026-09/free-var-bind-aliased-caller-lexical.md`; the pin is 22/22 green). Free-variable reads and writes were never the problem — a `:=` bind was carrying its cell into an intervening caller's env tier by name. For a compunit/mainline lexical ADR-0024's store supplies the lexical answer; for a lexical declared in a **bare block** there is no store, so the same two routes still reach a shadowing caller. Closing it is the env-model change ADR-0055 §7.5 disclaims, or the smaller step of extending the ADR-0024 capture to block scope. |
| [immutable-lvalues-that-mutsu-still-lets-you-assign-to](deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md) | L | **Drained hard this cycle and still worth mining.** Section C closed in full, and three of section B's four producers closed 2026-09-07 (`news/2026-09/slice-first-and-block-topic-element-containers.md`). What survives, all re-verified today: section A's six rows (`my @a := (1,2,3); @a.map({$_=5})` answers `(5 5 5)` where raku throws), section B's producer 1 (`@a.list.map({$_=7})` loses the write — a ONE-path difference inside `map`, located by breakpoint to `try_native_array_map` declining an `ArrayKind::List` receiver), C2 (`@a[1][0] = 9` autovivifies over a defined `Int`), D, E, F, and two more found while measuring producer 3. **Read "how the surviving rows differ" before designing**: two stated blockers were measured wrong, and the obvious runtime rule converts every section-B row into a spurious throw. The file's own conclusion is that closing B first is the cleaner order, and B is now most of the way there. |
| [dot-twigil-dot-assign-metaop-loses-its-rmw-origin](deep/dot-twigil-dot-assign-metaop-loses-its-rmw-origin.md) | S | **Down to one row, and renamed for it 2026-09-07.** Every `$.attr` read-modify-write is correct now across three entry points, including the `$.x++` forms that were still silently *over-mutating* — the file records that as the third wrong claim its predecessor made about the mutsu column. What survives, verified today: `$.s .= uc` on a non-`rw` scalar accessor dies `Cannot modify an immutable Str (a)` where raku answers `attr=a`. `.=` loses its RMW origin in the parser, so `$.s .= uc` and `$.s = $.s.uc` lower to the same `AssignExpr` while raku answers them differently. Smallest open `deep/` row. |
| [adr0039-slice2-container-reads-compile-to-a-slot](deep/adr0039-slice2-container-reads-compile-to-a-slot.md) | L | **No failing repro of its own — it is the enabler**, and it is here rather than in the Icebox because its blocker is measured closed and its acceptance criteria are written. A container read still compiles to a by-name `env` lookup where a scalar read compiles to `GetLocal(slot)`, and *that by-name read is what hides store-lane bugs*: two paths naming different containers under one name look fine as long as every read re-resolves. Third attempt; the file names four store-side defects and two roast rows measured on the *previous* build, and tells you to re-measure them. Do NOT reach for decl-site cell-boxing of every `@`/`%` as the repair — that regressed ~12 files through decont leaks. |
| [residual-try-cell-eager-seq-reification-divergences](deep/residual-try-cell-eager-seq-reification-divergences.md) | M | **Scope narrowed 2026-09-07 to two rows, and they are a different bug from the one the file was opened for.** ADR-0058 step 2 closed nine of eleven. The survivors use a `...` **stub** callback, which mutsu already deferred before ADR-0058, so eagerness was never their problem: with the enclosing `try` removed mutsu matches raku exactly. Add the `try` back and a force-time `fail` returns from the routine as a `Failure` where rakudo throws (verified today). Nobody has looked into that yet. |

### B2 — batteries / dist-blocking

| Ticket | Blocks | Effort |
|---|---|---|
| [vendor-real-test-module](deep/vendor-real-test-module.md) | making the vendored upstream `Test` the default (retiring a rung-3 native provider — a PLAN §1 goal) | L. **27.4s** on `roast/S03-buf/write-int.t` against a hard 30s gate on the file's own box, down from 48.0s two days ago; the two September fixes are **1.06 M → 535 k instructions per assertion**. **Re-measured for this regen on a ~3x faster box: 9.61s vendored / 1.92s native / 1.15s rakudo** (release, idle, exit 0, all 2 530 subtests) — i.e. **8.4x rakudo and 5.0x the native provider**, which reproduces the file's own ~8x ratio exactly. So the absolute second-count is a property of the runner, and **the ratio is the durable metric**: read "27.4 against 30" as "the gate is ~9% of margin on *that* box", not as a fixed number. Completion criterion 2 is still **not** met. The file names three perf rows as what closes it; **two of them have since been measured not to move this file at all** (see campaign 3), so the row it needs next has to be found by profiling, not read off the list. `t/` regressions: 0. |
| [config-toml-battery-core-blockers](deep/config-toml-battery-core-blockers.md) | `Config::TOML` (10/19) + `Crane` (3/15) battery slot | L (cluster). A self-re-measuring record that flags its own previous numbers as stale each pass — `Config::TOML 0/19` was badly stale and is 10/19. The dominant remaining Crane cluster (`X::OutOfRange`/`CATCH` re-throw descent) is honestly marked "not bisected". **Do not start the vendoring steps.** Note `tickets/itemized-hash-subscript-...` came out of this slot. |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | the template battery runner-ups | XL (cluster) — **re-measured 2026-09-06, trust these numbers**: `Template::Mustache` 13/13 and `Template6` 12/12 are **done**; `Template::Jinja2` 3/23, `Template::HAML` 39/83, `Template::Mojo` 4/5, `SP6` 10/11 (at parity), `Template::Nest::Fast` 0/10. The cheapest remaining lever is still `Template::Nest::Fast`'s single `with EXPR -> @m` list-wrapping bug, reduced to a one-liner in the file. Every `Template6` fix turned out to be a *general* interpreter bug, which is the pattern to expect. |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | re-landing #6499's `subtest` perf win | L — root cause still unknown; #6499 is still reverted (`subtest_call_block` still routes through `call_sub_value`). Class registration, LEAVE phasers and async transforms are all ruled out by measurement, which narrows it to early/conditional-response middleware meeting compiled-closure frame construction. Bisect from the dispatch end with `rust-gdb` frame diffs, not from Cro. Its escaped-class-registration half is now ADR-0047 and is *not* the fix. |
| [rakuast-remaining](deep/rakuast-remaining.md) | RakuAST parity | XL — an actively-worked ledger. Its own headline count is understated (`t/rakuast*.t` is **113 files** today against the 93 recorded on 2026-09-02), and ADR-0011's header is behind it. Zero roast dependents; pick by user impact, not cadence, and read the RakuAST implementation skill first. |
| [unify-statement-expression-control-construct-compilation](deep/unify-statement-expression-control-construct-compilation.md) | nothing directly — architectural debt | XL, and **measurably still getting worse**: `helpers_do_expr.rs` is **669 lines** (476 → 609 → 669), and `ForLoopSpec` is still constructed in two places. It keeps producing paired half-bugs. |

---

## Tier N — narrow correctness / diagnostics

| Ticket | Category | Effort / note |
|---|---|---|
| [ordered-alternation-eager-candidate-enumeration](deep/ordered-alternation-eager-candidate-enumeration.md) | correctness-narrow | L — **this is ADR-0073 Slice 2**, narrowed 2026-09-07 after slices 1+3 shipped. Only the `<subrule>` boundary is still collect-then-pick. Verified today: `regex TOP { <part> 'c' }` over `regex part { \w* {B} }` runs the block **5** times against raku's 2. The file now carries a 4-row measured table (E1/E2/E3/E6) plus four controls that must not move, and says why the `Named` arm resisted slice 1 (left-recursion seed loop + proto rank-then-match). **E3 is the cheap slice** — a ratcheted caller cannot backtrack in, so `first_only` fixes it outright and cuts waste on every bundled `token`/`rule` grammar. |
| [whenever-expression-position-needs-real-design](deep/whenever-expression-position-needs-real-design.md) | correctness-narrow | M — **down to one row.** ADR-0053 slice 1 landed: `do whenever` in expression position answers a real `Tap`, and every delivery shape agrees with rakudo. The survivor is `$tap.close`: rakudo prints `got 1 / got 2 / done`, mutsu prints `done` only — both emissions dropped, where the previous regen recorded only the post-`close` one. Overlaps ADR-0074. |
| [module-toplevel-private-sub-leak-cleanup](deep/module-toplevel-private-sub-leak-cleanup.md) | accepts code raku rejects | L — **confirmed, and a full implementation exists**: branch `fix/module-private-toplevel-sub-compunit-scope`, **PR #7436 (closed, not merged, +556/-137)**. Read `gh pr diff 7436` rather than re-deriving — its design (seclusion is a *move* into a per-compunit store, not a delete, because the module's own bodies reach their helpers through the same flat registry key) is sound and only its last mile is open. It is **gated on `tickets/qualified-call-falls-back-to-bare-global-routine`**: the assertion this wants to pin cannot be written while a bare `Pkg::name` falls back to the script's own sub. |
| [native-method-accepted-named-declarations](deep/native-method-accepted-named-declarations.md) | accepts code raku rejects | M — **no longer a live bug; it is hardening.** ADR-0070 slice 2 took the sweep from 78 to **18 of 1 422** probes (raku's own baseline is 23; only 9 of the 18 are mutsu-specific), and all three probes the previous regen listed as wrong now answer byte-identically to raku. What is left is `subst` and `trans`, which read their adverbs out of a slurpy — `trans` declares nothing but the implicit `*%_` and still reads `:d`/`:s`/`:c` out of it, which is why "the only slurpy is `%_`" cannot be used as a rule. Getting a row wrong here would break a widely used adverb, so it wants its own slice. |
| [metamodel-roles-are-not-composable-types](deep/metamodel-roles-are-not-composable-types.md) | missing feature | XL — **renamed and re-scoped 2026-09-07 after a THIRD framing closed under it**; the file keeps the history because the pattern is the point. Verified today: `class WithStashHOW does Metamodel::Naming { }` dies `X::InvalidType` before the body is read, where raku composes it. Two smaller residues (`.name` reports the added name; a plain `sub` as a method does not take the invocant as its first positional) are deferred with corpus evidence in the file. |
| [is-typename-custom-container-store-protocol-unimplemented](deep/is-typename-custom-container-store-protocol-unimplemented.md) | missing feature | XL — **confirmed**: `my @v is DNA = 1,2` never calls `STORE` and the logger never fires. The file rightly asks you to scope it first (grep the corpus for `method STORE`) before committing. |
| [slurpy-hash-named-arg-raku-boolean-shorthand-missing](deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) | rendering | L — **confirmed**: `foo :a1:b2` renders `{:a1(Bool::True), :b2(Bool::True)}`; raku `{:a1, :b2}`. Its own recommendation is "do not fix this in isolation" — it wants per-element `Hash` containers, the associative half of element itemization. |
| [begin-time-adverb-value-interpolation](deep/begin-time-adverb-value-interpolation.md) | correctness-narrow | L, **low priority by its own assessment** — `my $a:foo<42> = "answer"; say $a:foo«$c»` gives `Nil` where raku resolves it. No roast coverage, and the fix wants a whole-AST name-normalization pass across ~104 scattered `local_map` lookups. |

---

## Perf — batch into one profiling session; implementation agent runs SOLO

**Correcting the previous regen's headline.** It reported `bench-fib` going
"0.84x → 0.37x". Those are two different benchmarks. At `87e910a62`,
`bench-fib` was **0.82** and `fib` was **0.37**; today (`ce6b0576f`) they are
**0.86** and **0.40**. `bench-fib` has not moved at all. Read the benchmark
name as well as the ratio.

Current ratios at `ce6b0576f` (2026-09-07T02:56Z, from `bench-history.tsv` on
`bench-data`), interpreter row then `+jit`: `fib` 0.40 / 0.22, `bench-fib`
0.86 / 0.44, `hash-access` 0.18 / 0.19, `string-concat` 0.10, `bench-hash`
0.17, `method-call` 0.76 / 0.74, `time-parts` **1.06** / 0.63, `debug-guard`
0.81 / 0.66, `bench-tak` 0.94 / 0.57, `bench-ctor` 0.63 / 0.64. **`bench-ctor`
is the noisy one** — it printed 0.97, 0.60, 0.71, 1.00, 0.98, 0.95, 0.88 and
0.63 across eight consecutive main pushes on 2026-09-07 alone, so do not read a
single `bench-ctor` row as a signal.

| Ticket | Status |
|---|---|
| [non-constant-defaults-still-forfeit-the-light-path](perf/non-constant-defaults-still-forfeit-the-light-path.md) | **The residue of `defaulted-param-forfeits-the-light-call-path`, which CLOSED during this regen** (#7452: tiers 1+2 landed — constant defaults and bare `?` now fill from a registration-time table, 1001 full resolves over 1000 calls → **1**, and 13.8x fewer instructions on a defaulted-param loop). Tier 3 is what is left: compiling default *expressions* into a callee prologue, a compiler change rather than a binder one, which also retires `eval_param_default`'s **compile-afresh-on-every-call**. **Survey before building**, and note the correction it carries: the `Test`-module motivation the original ticket was ranked on is **measured stale** — `ok` is a multi and `proclaim` carries `is copy` plus a coercion, so neither becomes eligible and `write-int.t`'s resolve count is unchanged either way. |
| [method-dispatch-flattens-the-env-on-every-call](perf/method-dispatch-flattens-the-env-on-every-call.md) | **Read the "Update (2026-09-06b)" section FIRST — the obvious fix is measured neutral-to-negative.** Removing the `flatten_scoped_env()` guard and moving it to the three consumers that iterate was implemented in full, validated over both suites, and did not pay: with the guard in place the env at those sites is already flat, so the O(env) walks the move removes are the cheap kind. The prize also keeps shrinking (17% when filed → 10% after the return-merge fix). A real fix is either a persistent/HAMT env representation or removing the per-call need for a full view (three separate tickets). Re-run the `MUTSU_NO_FLATTEN` kill-switch before anything. |
| [listop-call-bypasses-every-compiled-call-cache](perf/listop-call-bypasses-every-compiled-call-cache.md) | **MEASURED, and the headline was wrong — read the file before picking it up.** The counter it asked for confirms 200/200 assertions take the carrier arm, but callgrind puts the *whole* carrier arm at **1.2%** of the run, and `ok`'s defaulted `$desc` makes it light-ineligible so a cache hit would land where the carrier already lands. The 8.5% the profile did find (`push_multi_dispatch_frame`'s registry-wide candidate walk) is **fixed**, −7.5% wall clock. What is left is the corrected budget table: `bind_function_args_values` 13%, `Env::insert_sym` 6.3%, `type_matches_value` 5.7%, `eval_param_default` 5.7%, ~47 `format!` per assertion. Wire the cache in as a **slow-path retirement**, not expecting a speedup. |
| [hash-copy-allocates-a-string-per-key](deep/hash-copy-allocates-a-string-per-key.md) | **NEW (filed under `deep/`, not `perf/`, because the fix is a type change across the codebase).** `HashData::map` is a `HashMap<String, Value>`, so every value-copy of a hash allocates one `String` per key — verified by scaling: +8 996 allocations for 9 extra keys × 1000 constructions, i.e. exactly one per key per copy. Raku assignment semantics make hash copies routine (`%a = %b`, binding a `%` parameter, storing into a `%` attribute). `Arc<str>` is the likelier answer than `Symbol` (arbitrary runtime keys would grow the symbol table without bound). **Measure before committing**: on `bench-ctor` it is 11 of ~1.40 M allocations; the prize is in JSON/YAML/META6-shaped workloads. |
| [adr0019-g3-diffuse-bless-allocation-cost](perf/adr0019-g3-diffuse-bless-allocation-cost.md) | **ACTIVE, and its next two items have both been re-scoped by measurement.** `mfast:epilogue` is **fixed** (4.9 → 2.2 allocations/call; a `TWEAK` was merging 26 unchanged entries back into its caller). `bless:named-args` was **misdiagnosed** and closed out — the 11 allocations are the hash-copy row above, not the linear scan the file guessed. What is left: the locals-init `format!` (reuse one scratch `String`), and **`mfast:body` at 40.7 allocations per method call exclusive — by far the largest region and entirely unexamined**. Sub-scope it by opcode family before saying anything about it. |
| [interpreter-call-path-in-hot-loops](perf/interpreter-call-path-in-hot-loops.md) | **Do NOT start from its "Where to start" — the file says so itself.** The `&`-sigil gate it blamed for a year is measured closed. The live finding: under `MUTSU_REAL_TEST=1`, **83.3% of function-call opcodes fall back to the interpreter**, dominated by `nqp::`-prefixed calls that never reach a cached dispatch despite being fixed known names. Methodology: "raku will delete your benchmark" — an unread accumulator is optimized away and manufactures a 140-370x fake deficit. |
| [late-august-call-path-slowdown-remainder](perf/late-august-call-path-slowdown-remainder.md) | **ACTIVE — the campaign's central ledger**, items closed inline as they land. Open item #3: `mutsu_jit_1` got ~50% slower since August with nothing in the interpreter explaining it — dump the generated code for both builds. Its "Do NOT keep bisecting" paragraph is still the most important one in `todo/perf/`. |
| [locals-frame-is-a-pooled-vec-not-a-register-window](perf/locals-frame-is-a-pooled-vec-not-a-register-window.md) | **BLOCKED on an ADR, and still the largest single remaining cluster** (~5.7% of `bench-fib`'s profile managing a one-element `Vec`: `recycle_locals` 2.36%, `Vec::drop` 0.96%, `extend_with` 0.90%, `drop_in_place<[Value]>` 0.85%, `Vec::resize` 0.68%). 484 `self.locals` sites across 60 files, `mem::take` load-bearing in three call paths, the JIT emitting against the current offset. Write the `Proposed` ADR (representation, migration order, JIT layout) before any code. |
| [hash-access-diffuse-regression-2026-09](perf/hash-access-diffuse-regression-2026-09.md) | **ACTIVE.** Ratio crept 0.17 → 0.19 with no single culprit; three fixes landed (266.2 M → 253.3 M instructions, −4.8%) and the rest is still there (0.18 today). Next: symbol-key the remaining ~155k `Symbol::intern` calls. Methodology, and it is the reusable part: **read the ratio column, not `mutsu_median_s`** — a 26% "step" was runner noise that moved raku's baseline identically, and cost two release builds to discover. |
| [hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing](perf/hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing.md) | **RECORD**, deliberately not a fix: GC ≈14%, allocation ≈12%, NaN-box decode ≈13%, key hashing+comparison ≈10%. Most tractable lead is `Interpreter::current_package()` (an `RwLock` read + `String` clone, 2.2%, 228 sites, with `current_package_sym()` already beside it) — get a caller breakdown first, most sites are cold. Leads 2 and 3 (SipHash→FxHash; whether a plain-scalar hash element needs a cycle-collected cell) both **need an ADR**. Overlaps the hash-copy row above. |
| [bench-ctor-construction-parity](perf/bench-ctor-construction-parity.md) | Round 5 disproved rounds 2-4's "flat profile" conclusion by finding a per-call `.map` **compile** (−12.9%). Still-open lead: custom-`new`→`bless` plumbing. Largely superseded in substance by the ADR-0019 G3 row; keep it for its methodology note (**`add_constant` must stay flat on a steady-state loop** — growth with iteration count means something is being *compiled* per call). |
| [yaml-parse-throughput](perf/yaml-parse-throughput.md) | **Round 10 closed the gap**: `bench-yaml-parse` 1.14s → 0.138s, now faster than rakudo. Remainder is genuinely flat (allocator ~20%, memcpy 6.7%, SipHash on capture maps 8.1%). Carries the "compare against rakudo, not against yourself" lesson: three rounds of internal profiling missed a 40x-over-firing grammar action that instrumenting *both* implementations found immediately. |
| [closure-literal-creation-cost](perf/closure-literal-creation-cost.md) | Parts A and C done (−20%/creation, body shared). **Part B needs an ADR**: narrowing `capture_closure_env`'s kept set trusts an incomplete static analysis — the `roles-6e.t` flake shape CLAUDE.md warns about. Cost the "share the system-name portion through the parent chain" alternative first. Methodology: `bench-startup.raku` is too short to A/B; consecutive runs of one binary differ ~2x. |
| [digest-ripemd-start-per-block-overhead](perf/digest-ripemd-start-per-block-overhead.md) | **Re-measure before anything.** 156.6s against a 120s gate, but its round-6 fix (generation-checked dispatch memos) is the same shape as the multi-resolution cache that has since landed generally, so the recorded number has almost certainly moved. Profile is otherwise flat. |
| [interpreter-new-is-expensive-and-retains-memory](perf/interpreter-new-is-expensive-and-retains-memory.md) | ~9.17ms and **~7.2 KiB retained** per `Interpreter::new()`, linear over 4000 constructions. **Debug numbers.** Nothing is currently blocked (the LSP's S2 avoided needing an `Interpreter`), but the retention is unexplained — chase the *retention*, not the wall clock. |
| [closure-sequence-evolution-performance-gap](perf/closure-sequence-evolution-performance-gap.md) | ~84x raku — **debug**, pure hypothesis, never profiled. The actionable signal is that the combined case (48s) far exceeds the sum of its parts (~7.5s). Profile `max :by` and `subst`'s regex-assertion closure crossing on release. The sibling bigint row closed this cycle by doing exactly that, so this one is next in kind. |

Numbers that end up in a document must come from the **bench CI**
(`bench-history.tsv` on `bench-data`), never from a profiling session's own
local runs. The ratios above are read from that file at `ce6b0576f`; everything
else quoted here is session-local routing evidence.

---

## Icebox — blocked on a decision, or a pure record

| Ticket | Blocked on / why |
|---|---|
| [containerref-holding-a-hash-is-indistinguishable-from-itemization](deep/containerref-holding-a-hash-is-indistinguishable-from-itemization.md) | **NEW, and already stopgapped — no live divergence today** (verified: its repro dies in both implementations). A `$`-scalar container holding a hash and a destructuring-slice element container are both spelled `ContainerRef(<Hash>)`, so `build_hash_from_items_with_key_coercion` cannot decide whether to flatten. The staging temp is excluded from element-container promotion, which removes the one reachable collision; the next producer that hands a `Hash`-valued element container into a hash initializer hits the same wall. The real fix — itemization living in the value, not the wrapper — touches every `hash_is_itemized` reader and the `${:a(1)}` vs `{:a(1)}` rendering. **Wants an ADR paragraph, not an opportunistic change.** `t/hash-itemization-flag.t` test 8 is the tripwire. |
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | Headline defect fixed (ADR-0055 slice 1b) and re-verified. What remains is structural — no `merge_all` equivalent in `call_compiled_closure`, and two disjoint per-instance state stores — with no measured wrong answer. Both are ADR-0055 slices 3-5, which *retire* the parameter rather than add a knob. Do **not** re-open its "gap 1 is observably wrong" framing; the population it could misresolve is now empty for plain scalars. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | Title is badly stale: all **373** rakudo `X::` subtypes now match, and the file regenerates its own numbers from a checked-in script. Only ADR-0029 R5 (the real-`Test` sweep) is left and it is blocked entirely on `vendor-real-test-module`. Closed-pending-a-dependency. |
| [lsp-references-needs-a-side-table-not-ast-spans](deep/lsp-references-needs-a-side-table-not-ast-spans.md) | **Measurement before design.** ADR-0065 D6 assumed spans on AST variants; the parser already knows every byte offset, so a thread-local occurrence table gated on an analysis flag is cheaper and touches neither `Expr`'s size nor the bincode cache. The blocker is **backtracking** — a `Var` parsed in a failed alternative is a phantom reference. Build the table behind the flag, run it over `modules/`/`vendor/`/`t/`, measure the phantom rate; that number decides the design. Also needs an explicit ADR decision that `references` is name-based. |
| [role-body-placeholder-mu-supply](deep/role-body-placeholder-mu-supply.md) | **Its own assessment is "don't"**: corpus hits across `roast/`, `modules/`, `vendor/`, `lib/` are **zero**, and the semantics being matched are garbage (rakudo supplies an uninitialized value whose `.defined` throws). ADR-0048 P5's value half; do it only if the deferred-body plumbing is opened for another reason. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | E3/E4 closed; E2 is a non-gating counter cleanup. ADR-0019's four completion gates are all closed. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Measurement record with reopen conditions. Blocker 3 (parser) is gone. Blockers 1/2/4 (QAST surface, MoarVM dispatch programs, 61 missing `nqp::` ops) stand, so `NativeCall` remains a justified rung-3 provider under BATTERIES.md. |
| [p5tie-stash-bind-key-protocol](deep/p5tie-stash-bind-key-protocol.md) | A corpus-measured deferral, not a bug: `Stash.BIND-KEY`/`CALLER::.BIND-KEY` are unimplemented and cover ~0.5% of sampled dists (`P5tie`, `annotations`). Rung-2 machinery only; ADR-0013 §7's `ContainerRef` is the proposed surface. |

---

## Housekeeping notes

- **Closed since the 2026-09-06 regen** — 35 files across 54 merged PRs
  (#7398-#7453): both Tier S rows' worst faces, 16 `tickets/`, 17
  `deep/` and 2 `perf/`. Highlights: the baggy bigint weights, the definiteness
  HOW (ADR-0069), the `.WHICH` identity pair, `$.attr` RMW, END phasers at
  compile time, `Supply` fan-out (ADR-0074), resumable `CATCH` (ADR-0072),
  native operators as dispatch candidates (ADR-0071), the custom `IO::Handle`
  routing, the zip-chain parse, the triangle-reduce laziness, `$*TOLERANCE`,
  the shaped-array element type, the miri leak flake, the bigint arithmetic
  deep-clone, and — in the four PRs that landed mid-survey — the enum `EVAL`
  result, the lazy-pipe triangle reduce, unreached-END lexicals, and constant
  parameter defaults reaching the light call path. Details are in
  `news/2026-09/` (223 files).
- **Four rows closed while this file was being written** (#7450-#7453), which
  is the routing hazard of a day this fast: a row read as "next" can be gone
  before the session that reads it starts. Two of the four filed a narrower
  successor in exchange (`tickets/an-our-declared-in-a-never-run-block-...`,
  `perf/non-constant-defaults-...`), both ranked here. **Before starting any
  row below, `git log --oneline -20` and check its file still exists.**
- **`todo/` files whose own root-cause or status section is wrong** — this
  project's most common failure mode, so treat it as the default assumption.
  This cycle the pattern showed up in a new place: **a file's own successor can
  be wrong too.** `deep/metamodel-roles-...` has now had **three** framings
  closed under it, each measured wrong or already fixed before the next was
  written; `deep/dot-twigil-...` had three wrong claims about the mutsu column,
  the last of which (`$.x++` "no longer over-mutates") was hiding the family's
  last silent data loss. Also this cycle:
  `tickets/array-and-hash-which-collides-...` (its original "content hash"
  diagnosis is wrong and would send you to the wrong code — the file says so),
  `deep/native-method-accepted-named-declarations` (its three named probes now
  pass), `deep/containerref-...` (its repro now agrees in kind),
  `deep/whenever-...` (symptom moved again),
  `deep/exception-class-hierarchy-...` (title vs body),
  `tickets/rustdoc-doc-link-warnings` (210 → 251), and
  `perf/interpreter-call-path-in-hot-loops` (says so itself, in bold).
  ADR-0041 §6 remains the model for recording this: keep the document, add a
  section naming which premises were measured false and why.
- **A wall-clock gate recorded without its calibration is not portable.**
  `deep/vendor-real-test-module`'s "27.4s against a 30s budget" reads as ~9% of
  margin; the same file on the box used for this regen runs in 9.6s, because
  that box is ~3x faster (rakudo 3.49s → 1.15s on the identical file). The file
  does carry a calibration table and does say to re-measure — that is what made
  the check possible. Record the **ratio to rakudo on the same box** alongside
  any absolute gate figure, and re-derive the absolute one from the ratio.
- **The previous TRIAGE's own perf headline was wrong**, and the mechanism is
  worth naming so it does not recur: it read `fib`'s new ratio against
  `bench-fib`'s old one. `bench-history.tsv` has both a `fib` and a `bench-fib`
  series (and a `+jit` twin of each). Always quote the benchmark name with the
  number, and check that the two rows you are differencing share it.
- **Two ADR headers are behind their code**: ADR-0011 points at
  `rakuast-remaining` as its live inventory but its own timestamp predates five
  weeks of landed slices; ADR-0065's header says "S0 and S1 shipped" while its
  own phasing table marks S5a done. (ADR-0053's header was behind and is now
  accurate — it says "partially implemented".)
- **`docs/doc-diff-backlog.md` is out of sync in the direction the file itself
  warned about.** Its 2026-09-06 re-sweep snapshot (high-signal 296 → **108**;
  `mismatch` 184 → 74, `crash` 112 → 34, `match` 2195 → 2376) is still current
  as a *corpus* measurement, but its **Ticketed** section lists five findings
  whose tickets have since closed (`role-mixin-name-...`, `zip-chain-...`,
  `array-assignment-eagerly-reifies-...`, `tolerance-...`,
  `itemized-list-element-var-...`). Updating those rows is a small docs-only
  PR and is exactly what the previous regen asked for. Next untriaged
  high-signal files: `Type/Any` (3+1), `Language/experimental` (0+4),
  `Language/objects` (2+1), `Language/traps` (2+1).
- **A new ticket source outgrew doc-diff this cycle.** Nine of the twelve new
  tickets came from the **neighbourhood sweep of a fix in flight**, not from
  the backlog. That is a healthier feeder (the repros are already minimal and
  already oracle-checked), but it has a failure mode: the tickets arrive in
  clusters that share code, and filing them independently loses that. The five
  mixin tickets are the example — see campaign 4.
- **No duplicate rows this cycle.** The closest pair is
  `tickets/global-match-scan-...` and `deep/ordered-alternation-...`: both are
  ADR-0073 residue, but they are different boundaries (the `:g` scan loop vs
  the `<subrule>` boundary) and were split deliberately.
- Verification for this regen was run ad hoc from `tmp/` (gitignored); each
  ticket's own repro block regenerates it.
