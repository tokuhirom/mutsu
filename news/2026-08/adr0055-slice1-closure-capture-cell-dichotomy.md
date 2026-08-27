# ADR-0055 slice 1: the three unboxed capture families are closed, and the rest of the cell gap is now measured

[ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md)
set out to make a closure's free variable resolve to its own captured binding,
and sequenced the work as five slices. Slice 1 — "close the unboxed-mutated
residue" — has landed. Just as valuable is what the attempt to go further
measured: both of slice 2's prerequisites turned out to be things this ADR did
not anticipate, and both are now written down with reproductions.

## Re-measuring first

Every premise was re-measured against `main` rather than trusted, because the
ADR's slice 1 rested on a prerequisite ("make `cas` and the constraint re-check
chokepoint cell-aware") that two other ADRs had moved underneath it.

Both halves had in fact closed. ADR-0042 made a type constraint a property of
the *container*, so a write through a `ContainerRef` re-checks it; ADR-0062
anchored the atomic lane to the published value with the root store as the sole
mapping authority. The refusal those two facts were protecting was obsolete.

The three residual families themselves, however, were all still open. Each was
probed in both failure directions and the verdict confirmed with a
`rust-gdb -batch` breakpoint on the boxing site, which fired for none of them:
a type-constrained scalar (`my Foo $x`), an Array held in a `$` scalar, and a
Package-valued scalar all took a by-value snapshot, and all three were
hijackable by a same-named lexical in whatever frame happened to be calling.

## What landed

The `type_constrained_unboxable` refusal and the `Package`/`Array`/`Hash`
value-kind skips are gone from both boxing sites. `Sub`, `Proxy`, and the
`Seq`/`Slip` family keep theirs (`Proxy` permanently: FETCH/STORE must not be
hidden behind a cell).

The relaxation flushed out exactly one regression, and it was a real hazard
rather than a defect in the relaxation. `t/atomic-cell-shape-refusal-symmetry.t`
test 4 sequences a legacy-lane `cas` on an Array-valued scalar — which refused a
cell and parked the authoritative value in the name-keyed atomic lane — and then
a thread capturing the same name. With `Array` newly boxable, the capture seeded
a fresh cell from the now-stale slot and forked the binding in two. The fix is a
new refusal, `legacy_atomic_lane_owns`: while the legacy lane owns a name, the
two capture/declaration boxing sites decline to promote it. The seed-and-retire
protocol that makes promotion safe is confined to `atomic_scalar_cell` for a
documented reason (it alone runs synchronously in the thread owning the atomic
op), so the capture sites cannot borrow it — and a refusal can only ever cost an
optimisation, never correctness.

## The gap that is still open, and what it cost to learn

Slice 1's three families are a *subset* of the coverage ADR-0055 actually needs.
The invariant slices 2-5 depend on is ADR-0025 slice 2's: every
escaping-captured plain scalar is either **authoritative** or a **cell**. The
remainder of the gap is the population the vouch refuses but the mutation
analysis never saw — a capture of a name handed to a call
(`own_call_arg_sources`), or one mutated in place as a container. That
population has neither defence, and it is exactly ADR-0055 §1.2(b):

```raku
sub noop($v) { 1 }
my $b = "OUTER";
noop($b);                        # the vouch refusal
my $f = { $b };
sub collide() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
say collide();                   # raku: OUTER    mutsu: CALLER
```

The mechanism that closes it is one compile-time set — the exact complement of
the vouch within the escaping-captured set, wired into `box_captured_lexicals`
as an independent trigger. It was implemented and validated during this slice:
§1.2(b) returns `OUTER`, the whole `t/` suite and a full local `make roast`
stay green, and the #2749 canary does not move.

It was still **removed from the shipped slice**, because it drops six
whitelisted Cro::HTTP suites in the bundled-library gate — a CI step `make test`
does not run — with state leaking between sequential requests on one client
(`GET .../index.SHTML/index.SHTML/counter/echo/`, an accumulating path). A
three-way bisect over the vouch-refusal shapes showed the breaking population is
*precisely* the read-only call-arg-source population §1.2(b) needs, so no
narrowing keeps the fix and drops the regression. An env-gated trace of the
names the new trigger boxed points straight at `Cro::HTTP::Client.request` — its
`$url`/`$method` **parameters** and its recursive redirect call — which suggests
the real defect is a freshness gap the trigger merely exposed: a parameter
binding is not a vardecl, so nothing resets a stale cell in that slot on the
next invocation. Full record, including the bisect table and the boxed-name
trace: `todo/deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md`.

The lesson worth keeping: `make test` and even a full local `make roast` are not
sufficient coverage for a change to closure capture. The bundled-library gate is
where a real dist notices, and it should be run locally
(`MUTSU_BIN=target/release/mutsu scripts/battery-testsuite.sh`) before pushing
anything in this area.

## What slice 2 still needs

The merge flip was implemented as specified, run against the full `t/` suite,
and reverted. Two things came out of it, both recorded in ADR-0055 §7.4.

A trap: the dynamic-variable exclusion must use the sigil-tolerant predicate the
`ContainerRef` branch already uses, not `env::is_dynamic_var_name` — env keys
reach the merge both bare (`*OUT`) and sigilled (`$*OUT`), and the latter is not
recognised, so the closure's captured `$*OUT` overwrote the caller's live
dynamic binding.

And the real news: with the flip in, exactly nine `t/` files still failed, and
every one of them was a single family — a capture the creator mutates later, for
which the escape/ownership analysis produces no cell. Three shapes: escape
through a *non-escaping* intermediate frame (`my $shared = 0; for 1..3 {
@cbs.push({ $shared }) }; $shared = 42`, where the requirement has to bubble two
frames up through a loop body that does not itself escape), a closure created
inside `EVAL`, and a resume-safe CONTROL handler's write into its installing
frame. Closing those is ADR-0025 slice 2's full decl-site design, which was
closed out on 2026-08-20 as "already resolved by intervening work" — a verdict
that was right about its motivating examples but left the mechanism unbuilt.
Slice 2's prerequisite list therefore reads: slice 1 (done) *and* that mechanism
(open).

Pin: `t/closure-capture-cell-dichotomy.t` (11 assertions, all raku-validated),
covering both directions for all three families, the slot-resident §1.2(b)
variant, both vouch-refusal shapes in the staleness direction, and the two
bounds the boxing keeps. The env-resident §1.2(b) variant is documented in that
file as the open half.
