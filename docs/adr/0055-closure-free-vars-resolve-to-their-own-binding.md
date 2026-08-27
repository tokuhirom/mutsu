# ADR-0055: A closure's free variable resolves to its own captured binding — retiring `merge_all` and the two closure-state stores

- Status: Accepted (slice 1 implemented 2026-08-28; slices 2-5 not started, and
  §7 records two prerequisites this ADR did not originally anticipate)
- Date: 2026-08-20 (renumbered 0054 → 0055 on 2026-08-20: two ADRs were
  authored concurrently as 0054 and this one lost the tie; the index row for
  0054 belongs to the argument-list-interpolation ADR)
- Related: ADR-0025 (cell boxing of captured scalars must be value-kind-blind —
  the mechanism this ADR builds on), ADR-0024 (mainline lexicals for named
  subs), ADR-0023 (binding provenance), ADR-0039 (container lexicals resolve
  lexically — the `@`/`%` half of the same principle), ADR-0013 (container
  interior mutability), ADR-0019 (compiled declarations and unified dispatch —
  the fork this unblocks)
- Addresses:
  `todo/deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md`;
  it also absorbs the "larger fix" that
  `todo/deep/eval-block-value-recompiles-every-call.md` named, which is this
  ADR's slice 4 — that ticket's own two costs were fixed by the
  `carrier_compile_cache` and it was retired on 2026-08-20 to
  `news/2026-08/eval-block-value-recompiles-every-call.md`. It also inherits the
  headline defect of the retired ticket
  `todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md` —
  the *original* statement of the §1.2(b) family, whose four tracked residuals
  were verified closed on 2026-08-20 (see
  `news/2026-08/closure-caller-hijack-residuals-closed-family-owned-by-adr0055.md`)
  while the family itself remained live. That ticket is where "a closure's
  read-only captured scalar is hijacked by a same-named lexical in the caller's
  env chain" was first written down; this ADR is now its only home.

## 1. Context

### 1.1 The finding as originally recorded

`todo/deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md`
records two structural obstacles to the long-planned fork of
`call_sub_value`'s general `ValueView::Sub(data)` branch (the ~400-line
tree-walk closure-call path in `src/runtime/resolution_call_sub.rs`) onto
`call_compiled_closure` (`src/vm/vm_closure_dispatch.rs`) whenever
`data.compiled_code` is `Some`:

- **Gap 1.** `call_sub_value(target, args, merge_all)` selects between two
  captured-env merge policies; `call_compiled_closure` has only one, and it is
  shaped like `merge_all == true`. There is no way to obtain the tree-walk
  branch's `merge_all == false` *default* (closure-value-wins for every
  non-authoritative free var) out of the compiled path.
- **Gap 2.** Per-closure-instance persisted state lives in two disjoint,
  non-communicating maps keyed by `data.id`: `closure_env_overrides`
  (`src/runtime/mod.rs:1362`, a whole-`Env` snapshot written by the tree-walk
  branch's `persist_closure_env`, `resolution_call_sub.rs:842-851`) and
  `closure_captured_state` (`src/runtime/mod.rs:1779`, per-free-var, written by
  `vm_closure_dispatch.rs:1104-1127`).

Both were re-verified against `main` at `b1a9bb8a5` (2026-08-20) and are
structurally intact. Two smaller findings from the same audit were filed
separately; one of them
(`todo/tickets/call-compiled-closure-underscore-arg-binding-bug.md`) has since
been fixed and retired, the other
(`todo/tickets/call-compiled-closure-missing-rw-lazylist-tail.md`) is still
open and is independent of this ADR.

Current scale: **131** single-line `call_sub_value(..., true)` sites across 44
files, and **109** single-line `..., false)` sites (plus a handful of
multi-line calls). `merge_all` also selects between two *exit* writeback
policies (`resolution_call_sub.rs:903-953` vs `:954-1016`), a fact the ticket
did not record — retiring the parameter means unifying both halves, not just
the entry merge.

### 1.2 What the investigation actually found

Re-running the ticket's premises produced a sharper — and different —
diagnosis. Three experiments, all dependency-free, all `raku`-validated:

**(a) The dual store is mostly inert, because cells already own the state.**
A counter closure invoked alternately through the compiled path (`.()`) and
through a `merge_all: true` native builtin (`.classify`) returns the correct
running count on `main`:

```raku
sub make-counter() { my $n = 0; return sub ($x) { $n = $n + 1; return $n } }
my $c = make-counter();
say $c(1); say $c(1);                    # 1, 2
say (9,).classify($c).raku;              # 3
say $c(1);                               # 4    (raku agrees on every line)
```

`rust-gdb` breakpoints on both persistence sites confirm the split really
happens — `resolution_call_sub.rs:849` fires twice (the two `classify` calls),
`vm_closure_dispatch.rs:1125` fires four times (the four direct calls) — and a
breakpoint on the tree-walk merge's `ContainerRef` branch
(`resolution_call_sub.rs:500`) explains why the answer is nevertheless right:
`$n` is **boxed into a shared `ContainerRef` cell** (ADR-0025 box-on-capture),
both paths force-overwrite `ContainerRef` captures, and both persistence stores
are *skipped* for cells (`vm_closure_dispatch.rs:459-467`). The cell, not
either store, is the source of truth. The same holds for the canonical
`merge_all == true` client: a `Proxy` FETCH/STORE pair over a backing lexical
takes the `ContainerRef` branch four times and behaves correctly.

So Gap 2's hazard window is not "any closure with mutable captured state" — it
is exactly the **unboxed residue**: captures that are mutated but that
ADR-0025's boxing still skips (slice 3's list: type/`where`-constrained
scalars, `$`-held Array/Hash, Package-valued scalars).

**(b) The compiled path's caller-priority default is observably wrong, and its
apparent correctness is an artifact of the dual store.** With a caller lexical
that merely shares a name, `call_compiled_closure` returns the *caller's*
value:

```raku
my $b = "OUTER";
my $f = { $b };
sub collide() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
say collide();                   # raku: OUTER    mutsu: OUTER   (control)
```

```raku
sub noop($v) { 1 }
my $b = "OUTER";
noop($b);                        # <-- the only added line
my $f = { $b };
sub collide() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
say collide();                   # raku: OUTER    mutsu: CALLER  (BUG)
```

The `my $g = { $b }` line is not decorative: it forces the caller's `$b` to be
materialised into `env` rather than living only in a local slot. Without it the
compiled path happens to answer `OUTER` — not because its policy is right, but
because a compiled caller usually keeps its lexicals in slots, so
`entry_or_insert_sym`'s chain probe finds nothing and the capture is installed
after all. The same closure invoked through a tree-walk `merge_all: true`
builtin answers `CALLER` even without the forcing line, because the native call
boundary flushes the caller's locals into `env` first. **The two paths differ
not (only) by policy but by whether the caller's lexicals happen to be visible
in `env` at the moment of the merge** — the `env_dirty` dual store leaking into
a *scoping* decision.

**(c) The load-bearing ingredient is a vouch refusal that cannot be made
complete.** `noop($b)` is what flips the result. `CompiledCode::compute_free_vars`
(`src/opcode.rs:5993-6018`) refuses to vouch for any lexical in
`own_call_arg_sources` — a name handed to a call — because an `is rw` parameter
could write it back, which would make an overwrite-install go stale. So `$b` is
simultaneously:

- **not authoritative** (handed to a call), and
- **not boxed** (never mutated, so `captured_mutated_locals` does not hold it
  and no cell forms),

leaving both defenses off and the caller free to hijack the name. The existing
pins for this family — `t/proxy-fetch-capture-vs-caller-lexical.t`,
`t/closure-readonly-freevar-live.t`, `t/closure-capture-instance-cell.t` — are
all green on `main`; this is a *new*, uncovered member of it.

### 1.3 Why this reframes the ticket

The ticket asks "how do we give `call_compiled_closure` a `merge_all` knob?".
The answer this investigation supports is: **do not**. Caller-priority-by-name
is a *staleness* workaround expressed as a *scoping* rule, and that category
error is the actual defect:

- The scoping question ("whose `$b` is this?") has one correct answer in Raku:
  the closure's own captured binding. ADR-0039 states the identical principle
  for `@`/`%` lexicals; ADR-0025 §"Alternatives rejected" already observed that
  merge-order tweaks can only fix hijack or liveness, never both.
- The freshness question ("is the captured value stale?") has one correct
  mechanism already in the codebase: the shared `ContainerRef` cell. Experiment
  (a) shows it is what actually carries the `Proxy` FETCH/STORE case that
  `merge_all == true` was introduced for.

ADR-0025's rejection of "merge-order / authoritative widening" was written
against a *narrower* cell population (before slice 1 removed the `Instance`
skip and before `cf9dc72be` made every method-call closure argument escaping).
Its counterexample, `my $s = 0; @cb.push({ $s }); $s = 42`, is a
*mutated* capture, hence boxed today, hence satisfied by the cell under
closure-wins. The rejection does not survive the current cell population; this
ADR supersedes it on that specific point and on nothing else.

## 2. Decision

**A closure's free variable resolves to the binding the closure captured. A
same-named lexical in the calling frame is a different binding and never wins,
on any call path. Freshness of a captured binding is delivered exclusively by
the shared container cell, never by name priority.**

Consequences that follow directly:

1. `call_sub_value`'s `merge_all` parameter is retired. Both the entry merge
   and the exit writeback converge on the single policy.
2. `authoritative_free_vars` / `authoritative_captures` / `frame_authoritative`
   stop being *correctness* gates. Under closure-wins every capture is
   installed with overwrite, so the vouch set has nothing left to select. (It
   may survive as a perf hint or be deleted; see §3 slice 4.)
3. `call_compiled_closure` becomes the one closure-call mechanism. The
   `call_sub_value` general branch forks to it whenever
   `data.compiled_code.is_some()`, mirroring the `compiled_routine` fork that
   already exists at `resolution_call_sub.rs:439-454`.
4. `closure_env_overrides` and `closure_captured_state` both become vestigial
   caches over the same unboxed residue, and are removed together once that
   residue is empty.

## 3. Slices

The order matters: closure-wins is only sound once every *mutated* capture has
a cell, so the cell work comes first.

**Slice 1 — close the unboxed-mutated residue (ADR-0025 slice 3, hardened
into a prerequisite).** Box the three families ADR-0025 §"Slice 3" still
skips: type/`where`-constrained scalars (requires making `cas` and the
constraint re-check chokepoint cell-aware), `$`-held Array/Hash, and
Package-valued scalars. Gate: for every captured lexical the creating frame
mutates after capture, a cell exists. Until this holds, closure-wins would
regress liveness exactly where ADR-0025 predicted.

**Slice 2 — make the compiled merge closure-wins.** In
`call_compiled_closure_with_topic` (`vm_closure_dispatch.rs:310-372`), replace
`entry_or_insert_sym` with `insert_sym` for plain captures, keeping the
existing dynamic-variable exclusion (`$*x` is dynamic-scope by design and must
keep resolving against the live frame — the `indir`/`$*CWD` case at
`:317-323`) and the `__mutsu_*` metadata exclusions. The four force-overwrite
loops that follow (`authoritative_free_vars`, `authoritative_captures`,
`owned_captures`, `cap_overrides` — `:402-479`) collapse into the main loop,
except `cap_overrides`, which is the unboxed-residue fallback and survives
until slice 5. Pin: the §1.2(b) repro, plus a variant where the caller's
lexical lives in a slot rather than `env`, so the fix cannot be satisfied by
the accidental slot/env asymmetry.

**Slice 3 — make the tree-walk merge closure-wins and delete `merge_all`.**
The `merge_all == false` entry policy (`resolution_call_sub.rs:529-536`)
already *is* closure-wins, so this slice is mostly deletion: drop the
`merge_all &&  !is_authoritative` branch at `:525-528` and unify the two exit
writebacks (`:903-953` and `:954-1016`) on the `merge_all == true` shape, which
is the more complete of the two (it propagates any changed value type, not just
the `Bool|Int|Num|Str|Rat` whitelist, and it is the one native-invoked
callbacks depend on). Then remove the parameter from all ~240 call sites
mechanically. The `Proxy` FETCH/STORE, `cas`, `Promise`, Supply-tap and
comparator families are the acceptance surface here — they are what
`merge_all: true` exists for, and slice 1 is what makes them safe without it.

**Slice 4 — fork `call_sub_value` onto `call_compiled_closure`.** With one
merge policy on both sides, add the `data.compiled_code.is_some()` fork next to
the existing `compiled_routine` fork. Gate the fork on the *closure instance*
(`compiled_code.is_some()`), never on `merge_all` or on the call site — a
per-instance-stable gate is what keeps a given closure's calls on one path and
one store. Retire `authoritative_free_vars`/`authoritative_captures` or demote
them to a documented perf hint. This is the slice the `eval_block_value`
recompile ticket wanted; note its own warning that a naive fork regressed
`roast/S04-declarations/state.t` 2.4x. That regression was root-caused on
2026-08-20 and is a *separate*, still-open perf prerequisite that must land
before the fork is measured:
`todo/tickets/nested-sub-in-block-otf-recompiles-per-call.md` — a `sub`
declared inside a block is absent from the `CompiledFns` the compiled-closure
path consults, so every call OTF-recompiles it
(`vm_call_func_ops.rs:1413`), and if it declares `state` it degrades further to
full tree-walk dispatch. Reproducible on `main` today via `.()` with no patch;
the cliff is invisible in `interpreter_fallbacks`, which is why the earlier
`RoutineScope` / `mainline_lexical_subs` hypotheses did not find it.

**Slice 5 — collapse the two state stores.** With the fork in place a closure
instance no longer straddles both paths, and with slice 1 the unboxed residue
is empty. Delete `closure_env_overrides` and `closure_captured_state`, and with
them `persist_closure_env`, `get/set/clear_closure_captured_state`, the
`gc_roots` visits (`gc_roots.rs:113` and `:215`), the thread-clone handling
(`runtime_thread.rs:522`, `:614`) and the two `.clear()` sites in
`builtins_system_async.rs` / `native_methods/system.rs`.

## 4. Consequences

- **Correctness.** The §1.2(b) family is fixed, and with it every shape where a
  read-only capture of a lexical that was handed to a call collides with a
  caller name. This is the residue ADR-0025 slice 2's close-out could not see,
  because its probes all used *mutated* captures.
- **Maintainability.** One closure-call mechanism, one merge policy, one state
  store. Three compiler-computed vouch sets stop being correctness-critical, so
  an incompleteness in the escape/mutation analysis can no longer produce a
  *wrong answer* — only a missed optimisation. That is exactly the
  "sound mechanisms that cannot go flaky" preference in CLAUDE.md.
- **Performance.** Neutral to positive on the merge itself (an unconditional
  `insert_sym` replaces a chain-walking `contains_key_sym` probe plus four
  follow-up overwrite loops). The fork in slice 4 is the actual perf prize and
  carries its own measurement obligation.
- **Risk concentrated in slice 1.** Everything downstream assumes "mutated
  capture ⇒ cell". If slice 1 leaves a family uncovered, slice 2/3 turn a
  hijack bug into a staleness bug for that family. The mitigation is that both
  directions are deterministic and roast-detectable — neither is a race — and
  slice 2 can keep `cap_overrides` as an explicit, documented fallback until
  slice 1 is provably complete.
- **A `merge_all`-shaped escape hatch must NOT be reintroduced.** If a call
  site appears to need caller-priority, the correct fix is a cell at the
  binding, not a knob at the call.

## 5. Alternatives rejected

- **Add a `merge_all` parameter to `call_compiled_closure` (the ticket's own
  proposal 1).** This propagates the category error into the mechanism that is
  supposed to replace the tree-walk path, and doubles the policy surface at the
  exact moment we are trying to halve it. It also cannot fix §1.2(b), which
  reproduces on *both* policies.
- **Leave the two stores separate and only keep a closure consistently on one
  path (the ticket's proposal 2).** Sound as far as it goes — a
  `compiled_code.is_some()` gate is per-instance stable — but it preserves two
  mechanisms for one job and leaves the unboxed-residue semantics undefined.
  Adopted only as the *interim* state between slices 4 and 5.
- **Fix the slot/env asymmetry instead** (make the caller's slots visible to
  the merge probe, so caller-priority becomes consistent). This makes the wrong
  answer *reliably* wrong: §1.2(b) would then return `CALLER` on every path
  rather than on some. It is the correct fix only if caller-priority were the
  right policy, which §1.3 argues it is not.
- **Keep caller-priority and widen the vouch instead.** `own_call_arg_sources`
  cannot be narrowed soundly from the creating frame: whether a callee's
  parameter is `is rw` is not knowable there in general (multi dispatch,
  code-object calls, `&`-params). An incomplete widening yields exactly the
  load-order-dependent wrong answers ADR-0024 and ADR-0025 both rejected.

## 6. Acceptance criteria

- Slice 1: for every captured-and-mutated lexical in the three residual
  families, a `rust-gdb` breakpoint on the `ContainerRef` branch of both merges
  fires; ADR-0025's pins stay green; `S17-lowlevel/cas.t` stays green.
- Slice 2/3: the two §1.2(b) repros return `OUTER`, committed as a `t/` pin
  alongside the slot-resident variant; `t/proxy-fetch-capture-vs-caller-lexical.t`,
  `t/closure-readonly-freevar-live.t`, `t/closure-capture-instance-cell.t`,
  `t/closure-captured-state.t`, `t/wrap-closure-capture.t` all stay green; full
  roast delegated to CI.
- Slice 4: no regression on `roast/S04-declarations/state.t` wall-clock
  (the ticket's 2.4x trap), measured against the bench CI series, not locally.
- Slice 5: `closure_env_overrides` and `closure_captured_state` are gone and
  `git grep` finds no reader.

## 7. Implementation status

### 7.1 Premises re-measured before implementing (2026-08-28, `main` @ `239c3b818`)

Every premise slice 1 rests on was re-measured rather than trusted.

**All three residual families were still open.** Each was probed with the
liveness shape (`my $v = <kind>; my $f = { read $v }; $v = <new>;`) crossed with
the §1.2(b) hijack shape, and the verdict confirmed by a `rust-gdb -batch`
breakpoint on the boxing site (`vm_register_ops.rs`, the
`cur.clone().into_container_ref()` line), which fired for none of them:

| family | probe | mutsu before | raku |
| --- | --- | --- | --- |
| type/`where`-constrained scalar (`my Foo $x`) | hijack | dies, `No such method 'v' for invocant of type 'Str'` | `42` |
| `$`-held Array (`my $a = [1,2]`) | hijack | `1` (the decoy's `.elems`) | `3` |
| Package-valued scalar (`my $p = A`) | hijack | `Str` (the decoy) | `B` |

The liveness direction passed on `main` for all three — but only by accident,
through the caller-priority merge, since the creating frame *was* the calling
chain. That is exactly the residue §4 warns closure-wins cannot tolerate, so
the measurement confirms slice 1 as a genuine prerequisite rather than
bookkeeping.

**Both halves of slice 1's own stated prerequisite had in fact closed.** The
type-constraint skip existed because a `ContainerRef` write-through bypassed
the constraint re-check and because `cas` resolved its target by name. ADR-0042
made the constraint a property of the *container* (`my Str $s; my $t := $s; $t
= 42` dies correctly today), and ADR-0062 anchored the atomic lane to the
published value with the root store as the sole mapping authority. Retiring
the skip cost nothing on `roast/S17-lowlevel/cas.t` (green, 0.73 s).

### 7.2 What slice 1 landed

- The `type_constrained_unboxable` refusal is gone from `box_captured_lexicals`,
  and `Package`/`Array`/`Hash` left the value-kind skip list there and in
  `box_decl_local_cell`. `Sub`, `Proxy` and the `Seq`/`HyperSeq`/`RaceSeq`/`Slip`
  family keep theirs (`Proxy` permanently — FETCH/STORE must not be hidden
  behind a cell).
- **New refusal: `legacy_atomic_lane_owns`** (`builtins_atomic_shared.rs`), the
  one regression the relaxation flushed out. `t/atomic-cell-shape-refusal-symmetry.t`
  test 4 sequences a legacy-lane `cas` on an Array-valued scalar (which refused
  a cell, parking the authoritative value in `__mutsu_atomic_value::N`) and then
  a thread capturing the same name. With `Array` newly boxable, the capture
  seeded a cell from the now-stale slot and forked the binding — precisely the
  mid-sequence promotion hazard
  `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md` documents. The
  seed-and-retire protocol that makes promotion safe is confined to
  `atomic_scalar_cell` for a documented reason (it alone runs synchronously in
  the thread owning the atomic op), so the two capture/declaration boxing sites
  simply DECLINE while the lane is live. A refusal can only cost an
  optimisation, never correctness, and the process-global "any atomic seen"
  flag makes it free in programs that use no atomics.

Effect: all three residual families are closed in **both** directions. Their
liveness stops being accidental (it no longer depends on the calling chain
containing the creating frame), and — because a `ContainerRef` capture takes the
merge's existing force-overwrite branch — the caller-hijack direction is fixed
for them too, with no change to the merge policy.

### 7.3 What did NOT land, and why it matters more than what did

The invariant slices 2-5 actually need is ADR-0025 slice 2's: **every
escaping-captured plain scalar is either authoritative or a shared cell.** Slice
1's three families are a *subset* of the gap. The rest is the population the
vouch refuses but the mutation analysis never saw: a capture of a name that was
handed to a call (`own_call_arg_sources`), or one mutated in place as a
container (`own_container_writes`). That population has neither defence, and it
is exactly ADR-0055 §1.2(b).

The mechanism that closes it — one compile-time set computed as the exact
complement of the vouch within the escaping-captured set, wired into
`box_captured_lexicals` as an independent trigger — was implemented and
validated during this slice. It fixes §1.2(b)'s env-resident repro outright
(`OUTER`, not `CALLER`), with `entry_or_insert_sym` still the merge default and
no perf movement on the #2749 canary. `make test` and a full local `make roast`
both stayed green with it in.

It was nonetheless **removed from the shipped slice**, because it drops six
whitelisted Cro::HTTP suites in the bundled-library gate (a CI step `make test`
does not run): state leaks between sequential requests on one client, visible as
an accumulating request path. A three-way bisect over the vouch-refusal shapes
showed the breaking population is *precisely* the read-only call-arg-source
population §1.2(b) needs, so there is no narrowing that keeps the fix and drops
the regression. Full record, including the boxed-name trace pointing at
`Cro::HTTP::Client.request`'s parameters and its recursive redirect call:
`todo/deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md`.

**Consequence for §3.** Slice 2's prerequisite list should read: slice 1 (done)
**and** ADR-0025 slice 2's mechanism (open, blocked on that ticket). §1.2(b)
therefore remains open in its env-resident form; the slot-resident form is
pinned and passing.

### 7.4 Slice 2 was prototyped, measured, and reverted — what it still needs

The merge flip itself was implemented exactly as §3 specifies
(`entry_or_insert_sym` → `insert_sym` in `call_compiled_closure_with_topic`,
keeping the dynamic and `__mutsu_*` exclusions) and run against the full `t/`
suite. Two things were learned:

1. **A trap for whoever lands it.** The dynamic-variable exclusion must use the
   sigil-tolerant predicate the `ContainerRef` branch already uses
   (`s.trim_start_matches(['$','@','%','&']).starts_with('*')`), NOT
   `env::is_dynamic_var_name`. Env keys reach the merge both bare (`*OUT`) and
   sigilled (`$*OUT`) depending on the write path, and `is_dynamic_var_name`
   returns `false` for the sigilled form — so the closure's captured `$*OUT`
   overwrote the caller's live dynamic binding and every `is captured({...})`
   style test lost its output.

2. **The remaining cell-coverage gap is wider than §3 recorded.** With the flip
   in, the trap fixed, *and* the §7.3 mechanism in place, exactly nine `t/` files
   still failed, and every one of them is a single family: *a capture the creator
   mutates later, for which the escape/ownership analysis produces no cell*.
   Three shapes:

   - **Escape through a NON-escaping intermediate frame.** `my $shared = 0; for
     1..3 { @cbs.push({ $shared }) }; $shared = 42` — the inner closure escapes,
     but the `for` body that owns the capture chain does not, and `$shared` is
     owned two frames up. `needs_cell_free_vars` bubbles only *mutated* captures
     upward, so a read-only inner capture of a later-mutated outer lexical
     bubbles nothing (`t/closure-upvalue.t` 8,
     `t/closure-capture-instance-cell.t` 6, `t/loop-bind-closure-capture.t` 5).
   - **A closure created inside `EVAL`** (`t/eval-read-caller-lexicals.t` 13).
   - **A resume-safe CONTROL handler's write into an installing frame**
     (`t/control-warn-resume-caller-var-name-collision.t` 1).

   Closing these needs decl-site boxing plus bubbling the cell requirement
   through non-escaping intermediates — ADR-0025 slice 2's *full* design. Note
   that ADR-0025 slice 2 was closed out on 2026-08-20 as "already resolved by
   intervening work": that close-out was correct about its *motivating
   examples*, but the mechanism it specified was never built, and the merge flip
   is what makes the difference observable.

### 7.5 Not addressed

Two findings routed to this ADR do not fall out of slice 1, and were re-measured
to confirm it (both still diverge from `raku` on this branch):

- `todo/deep/sigilless-alias-closure-capture-skips-typecheck.md` — a `:=`-bound
  alias stops aliasing when the write happens inside a *stored* closure. Not a
  merge-policy or cell-population problem: the alias identity itself is lost.
- `todo/tickets/free-var-read-in-callee-resolves-through-dynamic-caller-chain.md`
  — a callee's free variable resolves through the dynamic caller chain. Making
  free-variable resolution genuinely lexical is an env-model change and needs
  its own ADR; it is the same principle as this one seen from the read side.
