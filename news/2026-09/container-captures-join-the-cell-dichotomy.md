# Container captures join the cell dichotomy

ADR-0055's invariant — *an escaping-captured lexical is either **authoritative**
(a by-value snapshot is provably exact) or a shared `ContainerRef` **cell*** —
shipped for `$` scalars only. `@`/`%` container lexicals were excluded from
`CompiledCode::needs_cell_unvouched_locals` by an explicit sigil filter. They
have their own half of the complement now,
`CompiledCode::needs_cell_unvouched_containers`, and the `@`/`%` hijack is
closed for every shape but three measured bounds.

## The defect

```raku
my @a = 1, 2;
@a.push(3);
my $f = -> { @a.elems };
sub collide() { my @a = 9; $f.() }
say collide();          # raku: 3    mutsu: 1
```

`@a.push(3)` is load-bearing: an in-place container write is one of the two
shapes `own_container_writes` refuses to vouch for, so `@a` is not in
`authoritative_free_vars`. With no vouch and no cell, the closure-call merge's
don't-overwrite default handed the win to whatever same-named `@a` the calling
frame happened to declare.

## Where the ticket's diagnosis was wrong

The ticket claimed the caller's decoy closure (`my $g = { @a.elems }`) was
load-bearing, as it is in the scalar half — it is what forces the colliding
lexical out of a local slot and into `env`, where the merge's probe can see it.
It is not load-bearing here: a container declaration reaches `env`
unconditionally, so the plain slot-resident form above already diverges. The
family is much wider than the ticket's single repro. Measured on `main`, all of
these answered the calling frame's array:

- `%` as well as `@`;
- `@a[2] = 3` (element assign) as well as `@a.push`;
- `@a = 7, 8, 9` (whole reassignment) as well as in-place mutation;
- an `our @a`;
- the collision two frames up, inside a `for` body, through `.map($f)`, and
  through a nested closure;
- two closures from one factory called from a colliding frame (`1 2` became
  `99 99`);
- worst of all, a *mutating* capture: `-> { @a.push(4) }` pushed onto the
  caller's unrelated array and left its own untouched.

The ticket's stated reason for excluding containers — "an `@`-sigil capture
reaches the merge as a plain `Array` value that is already reference-shared, so
overwrite-vs-don't decides *which array object* the name resolves to, not
whether a mutation is visible" — describes the mechanism correctly and then
draws the wrong conclusion from it. Deciding *which array object a name resolves
to* is precisely the bug. Reference-sharing makes the captured **value** live
(pinned: a post-capture `push`, and even a whole reassignment, is visible to the
closure and survives the creating frame's return); it does nothing about **name
resolution**, and a plain `Array` capture carries no signal that makes any of the
three merge policies prefer the closure's own binding. `ContainerRef` is that
signal — every merge already force-installs one — so the container half needs the
cell for exactly the same reason the scalar half did.

## The fix, and why it is at the declaration site

`compute_free_vars` computes the container half of the vouch's complement into
the new `needs_cell_unvouched_containers`, and `exec_set_local_op` boxes those
names at their DECLARATION via `box_decl_local_container_cell` — ADR-0039's
mechanism, not the scalar lane's per-capture `box_captured_lexicals`.

The first attempt did put it in `box_captured_lexicals`, and that is where the
container lane stops resembling the scalar one. A closure creation op can run
orders of magnitude more often than the declaration it captures: `@o.shift xx $_`
builds one thunk per repetition, so a 1200-element drain created 720k closures
over 1.2k declarations. Boxing per capture also defeated the whole function's
early return, so every free variable of every one of those closures walked the
boxing loop. `roast/S15-nfg/concat-stable.t` went from 2s to 31s — past its 30s
budget, and red on all three CI roast jobs. One boxing per declaration costs
nothing measurable (that same file now runs in under a second).

## Three bounds, each found by a gate rather than by reading

**1. The cell is never published into `shared_vars`.** The scalar path mirrors a
new cell into that lane so a running thread's snapshot points at it. The lane is
NAME-KEYED and process-wide, so publishing declares "this cell is what `@in`
means" for every frame — the same hijack ADR-0055 §7.7 traced for a captured
parameter, one sigil over, and it fires from the first `start` *anywhere* in the
program. Text::CSV's own `my @in` published its cell, `sync_shared_vars_to_env`
wrote it over the caller's unrelated `@in`, and a test script's list of input
formats silently became the parser's row buffer: battery `Text::CSV/90_csv.t`
went from 524 passing subtests to 133.

**2. A container handed to a THREAD is excluded from the set.**
`assign_array_elem_to_shared_var` deliberately stands down for an already-celled
container and defers to the general assignment path — which mutates the
`ArrayData` behind the cell *without* holding the cell's Mutex. Boxing a
container that concurrent `start` blocks write therefore converts a locked
read-modify-write through the `__mutsu_atomic_arr::` lane into a genuine data
race: `t/concurrent-array-index-assign.t` and `t/concurrent-hash-assign.t` abort
with `internal error: with_array_mut probed an Array` (a TOCTOU inside
`with_array_mut`) and a glibc `double free or corruption`. That is pre-existing
UB this change would have walked into. `thread_escaping` is already transitive on
each nested `CompiledCode`, so a `start` at any depth is covered.

**3. A container that carries an `is <Type>` trait, or an element type
constraint, is excluded.** `my %h is BagHash = a => 1, b => 0, c => 2` builds a
plain Hash at the declaration store and lets `ApplyVarTrait` coerce it
afterwards, reading the slot back to find the initial values; a cell in that slot
is not the Hash it looks for, so the initialiser was dropped and `%h` came out
with one key instead of two (`roast/S02-types/baghash.t`, `mixhash.t`, 24
subtests between them). Lifting `box_decl_local_container_cell`'s existing
typed-container refusal for this trigger was tried and measured wrong, so the
refusal stands and an `ApplyVarTrait` name is subtracted from the set at compile
time. Note that exclusion is by NAME across the whole frame, because same-named
`my` locals share one slot.

Bounds 2 and 3 leave the corresponding captures hijackable. Both are pinned as
such, and the thread one is written up with its two possible closures in
`todo/deep/celled-container-element-write-races-under-threads.md`.

## Two other bugs the cell exposed

**`squish`'s eager pass.** `.squish(:as, :with)` runs its callbacks eagerly and
reverts their env side effects when `.iterator` replays them lazily. The revert
detaches `Array`/`Hash` values so its changed-value diff can see in-place
mutation, but did not know the `ContainerRef` shape — where the binding never
changes because the cell *is* the binding — so the revert silently became a
no-op. `roast/S32-list/squish.t` had been passing only by accident: mutsu calls
`:with` **8** times where raku calls **3** (pre-existing, provable on `main` with
a scalar counter, which has had a cell since ADR-0055), and the array
accumulator's extra pushes were being silently lost — one bug cancelling the
other. `unique_squish.rs` now snapshots and diffs a celled container's contents,
and `dispatch_iterator_method` restores them *through* the cell rather than
replacing the env entry (which would both leave the eager pushes standing and
detach every alias from the name).

**The `@var.shift xx N` bulk drain.** `try_bulk_shift_pop` turns a whole `xx`
repetition into one array split instead of N closure calls. It looked the array
up in `env` and bailed the moment it found anything that was not a bare `Array` —
so a celled container fell back to the per-repetition thunk, 33ms to 4s on a
1200-element drain. It derefs the cell now and writes the drained remainder back
*through* it, which also keeps every alias (and the owner's own slot, holding the
same cell) coherent — strictly better than the env-only write it did before.

**A same-named sibling block must rebind.** Same-named `my` locals share one
slot, so a second block's declaration finds the first block's cell sitting there.
Leaving it would hand two `my` bindings one container and leave `env` naming the
first block's leftover plain array, which the escaping closure captured instead
of the cell — the owner saw an empty container and the writes surfaced one block
late. The unvouched trigger therefore rebinds to a fresh cell, which is what a
`my` means.

## Verification

`t/container-capture-cell-dichotomy.t`, 23 assertions, byte-identical under
`mutsu` and `raku`: both sigils in the hijack direction (with and without the
caller decoy) and in the staleness direction (in-place, element-assign and whole
reassignment, including after the creating frame has returned), the mutating
capture, the typed-container bound, three invocation paths, the per-instance and
per-iteration freshness guards, sibling closures sharing one cell, an `our`
container, the sibling-block rebind, and the squish revert. Gates: `make test`, a
755-file targeted roast sweep over the closure/declaration/container/NFG/
concurrency synopses, and the bundled-battery gate (289/312, unchanged).
