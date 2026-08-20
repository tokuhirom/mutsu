# Scalar-cell promotion refuses Seq/Slip the same way it refuses Array/Hash

`box_captured_lexicals` (`src/vm/vm_register_ops.rs`), `box_decl_local_cell`
(`src/vm/vm_var_assign_local_get.rs`), and `atomic_scalar_cell`
(`src/runtime/builtins_atomic_shared.rs`) promote a captured or declared
scalar local into a shared `ContainerRef` cell so that a nested named sub or
a spawned thread observes later reassignments through one binding instead of
a by-value snapshot. All three refused to do this when the local's current
value was an `Array`, `Hash`, `Package`, `Sub`, or `Proxy` -- those are
already reference-shared (Gc-backed), so a scalar-level cell around them is
unnecessary, per the founding rationale in commit 5cedcfe60 ("closures
capture the container, not the value"). `Seq`, `HyperSeq`, `RaceSeq`, and
`Slip` were not in that list, even though they are exactly as
reference-shared (Arc-backed `SeqBody`/`Vec` bodies) as `Array`/`Hash`.

This asymmetry surfaced a real bug in a rejected experimental design: `cas
$acc, -> @c { flat @c, 1 }` called twice sequentially starts each round with
`$acc` holding an `Array` (refused -- stays on the legacy name-keyed atomic
lane), but `flat` returns a `Seq`, so by the second round `$acc` held a `Seq`
and got promoted to a cell mid-sequence, silently switching cross-thread
reconciliation mechanisms for the same variable without retiring the legacy
lane's stale mapping.

## What changed

**Seq/HyperSeq/RaceSeq/Slip now share the Array/Hash/Package/Sub/Proxy
refusal** in all three promotion sites, closing the specific asymmetry.
Verified empirically (not just by rationale) that nothing depends on the old
inclusion: a captured `Seq` or `Slip` reassigned inside an `await`ed `start`
block stays coherent through the general `shared_vars` cross-thread reconcile
even without a cell (`t/atomic-cell-shape-refusal-symmetry.t` subtests 2-3),
under both the default and `MUTSU_NO_BLANKET_RECONCILE=1` reconcile modes,
and the full local atomic/cas/cross-thread/shadow-slot/whenever test corpus
(57 files, 405 assertions) plus the whitelisted `roast/S17-lowlevel/*.t`
files passed unchanged with the exclusion in place.

`atomic_scalar_cell` additionally had a narrower, pre-existing ordering bug
in its own seed-and-retire step (present before this change, unrelated to
Seq/Slip): it retired the legacy lane's mapping *before* checking whether the
seeded value's shape would actually pass the refusal check, so a refused
shape silently lost the value instead of leaving the lane alone. Fixed by
moving the retire step to after the shape check.

## An attempted symmetric fix was reverted after it broke a whitelisted roast test

The first version of this fix also taught `box_captured_lexicals` and
`box_decl_local_cell` to run the same seed-and-retire protocol
`atomic_scalar_cell` uses, on the theory that all three promotion sites
should treat a same-name legacy-lane entry as more authoritative than a
possibly-stale local slot. That regressed `roast/S17-lowlevel/cas.t`'s "CAS
on linked list with lexical head works" subtests: the test spawns 4 racing
worker closures under one bare-named lexical (`for 1..4 -> $attempt { my
$head = Node; await start { loop { cas($head, $orig, $next) } } xx 4 }`).
`box_captured_lexicals` and `box_decl_local_cell` fire at closure-creation
time, which -- unlike `atomic_scalar_cell`, which only ever acts on the
CURRENT thread's own atomic op on its own declared local -- can race with an
ALREADY-RUNNING sibling thread that is actively using the SAME bare name's
legacy-lane mapping. Seeding from the legacy lane there boxed a value a
*different* thread had produced instead of this frame's own current value,
and retiring the mapping there could rip it out from under that other
thread's in-flight retry loop. Both closure-creation sites were reverted to
only add the Seq/Slip refusal, with no legacy-lane interaction at all; only
`atomic_scalar_cell` (same-thread, own-local, no cross-thread race) keeps the
seed-and-retire protocol.

## What is still open

Investigating this surfaced a deeper, unrelated bug: a thread whose own `env`
snapshot predates a `reset_atomic_var_key` call can resurrect a stale legacy
atomic-lane mapping for the same name, and the general `shared_vars` blanket
reconcile can then clobber a strictly newer value with that stale one -- a
lost-update race that reproduces with plain scalars and no cell involved at
all, given an explicit `Channel`-forced thread interleaving (it does not
arise from the ordinary sequential-`await` pattern this fix's tests use, nor
from the racing-closures shape that regressed `cas.t` above -- that one is a
different, already-fixed hazard). This is a materially different, harder
problem (a last-write-wins-by-name flaw in the general cross-thread
reconcile, not the promotion refusal list) and needs its own design work: see
`todo/deep/stale-env-thread-can-resurrect-legacy-atomic-lane-mapping.md`.

Pin: `t/atomic-cell-shape-refusal-symmetry.t`.
