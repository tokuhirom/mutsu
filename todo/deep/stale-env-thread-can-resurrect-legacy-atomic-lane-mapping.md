# A thread whose `env` predates a shared-var write can resurrect a stale legacy atomic-lane mapping and clobber a fresher value via the blanket reconcile

Found 2026-08-20 while investigating
`todo/tickets/atomic-cell-shape-refusal-asymmetry.md` (now resolved --
Seq/HyperSeq/RaceSeq/Slip were added to the scalar-cell promotion refusal
list, and a seed-and-retire protocol was added to
`box_captured_lexicals`/`box_decl_local_cell` to match `atomic_scalar_cell`'s
existing one).

## The observation

`atomic_value_key_for_name` (`src/runtime/builtins_atomic.rs`) lazily
allocates a `__mutsu_atomic_name::<name>` -> `__mutsu_atomic_value::N` mapping
in the **process-global** `shared_vars` root store the first time a name is
used atomically, and caches the mapping in the calling frame's own `env` too.
`reset_atomic_var_key` (`src/runtime/runtime_shared_vars.rs`) removes both
entries on every plain scalar assignment and on the seed-and-retire step the
sibling ticket above added.

The bug: a thread whose OWN `env` snapshot predates a `reset_atomic_var_key`
call (because its closure was captured before that assignment ran) does not
see the mapping was removed. When it later performs its own atomic op
(`cas`/`atomic-*`) on the same bare name, `atomic_value_key_for_name` finds
nothing in its own `env` OR the (now-cleared) root store, so it happily
allocates a **brand-new** `name_key`/`value_key` pair and writes it back into
the root store -- **resurrecting** a mapping for that name after another
thread believed it was gone. If that thread's operation reads its own
(possibly very stale) `env` copy as the "current" value and writes it back,
it also marks the bare name dirty in the process-wide `shared_vars_dirty` set.

`sync_shared_vars_to_env`'s blanket reconcile (`src/runtime/runtime_shared_vars.rs`)
resolves a dirty name by looking up `__mutsu_atomic_name::<name>` in the
**live** root store -- which now finds the stale thread's freshly-resurrected
mapping -- and unconditionally overwrites the awaiting thread's `env` (and, if
a cell exists for that name, writes *through* the cell too) with the stale
thread's value. This silently clobbers a strictly newer value with older
data: a lost update, independent of cells or the Array/Seq asymmetry -- it
reproduces with plain scalars and no cell involved at all.

## Minimal repro (requires `Channel`-forced ordering; this doesn't arise from
## ordinary sequential-`await` code, only from a thread that starts before a
## write and executes its own atomic op after it)

```raku
my $x = [1, 2, 3];
my $go = Channel.new;

# Thread B's closure captures $x's pre-write env NOW.
my $pB = start { $go.receive; cas $x, -> $v { $v } };

# A same-thread priming cas + a later change to $x (any mechanism -- another
# cas, a plain reassignment, or a cell write) that happens AFTER $pB was
# spawned but that $pB's own env snapshot cannot see.
cas $x, -> @c { @c };
$x = flat($x, 4);
Promise.allof(start { $x = flat($x, 5) }).result;   # $x is now '1 2 3 4 5'

$go.send(1);
$pB.result;   # Thread B's stale cas resurrects a mapping and the blanket
              # reconcile clobbers $x back down to '1 2 3' here.

say ~$x;   # '1 2 3' instead of '1 2 3 4 5' -- WRONG.
```

## Why this wasn't fixed in the sibling ticket

The sibling ticket's fix (seed-and-retire on promotion, and excluding
Seq/Slip from cell promotion) closes the *specific* asymmetry it was tracking
-- a stale legacy-lane VALUE surviving past a cell promotion for the *same*
still-running frame. It does not, and structurally cannot, fix this: the
resurrection happens through a *different* thread's `env`, which no amount of
retiring-on-promotion in the writer's own frame can reach. This needs the
reconcile side (or the resurrection side) to change, which is a materially
different, harder problem -- likely requiring either (a) a generation/version
counter on the legacy-lane mapping so a stale thread's re-creation can be
detected as stale rather than authoritative, or (b) refusing to let a thread
whose `env` never observed a given name's mapping create a *new* one at all
once the process has seen that name go through at least one reset (forcing it
onto a different, explicitly "I am stale" path instead).

## Why this is `todo/deep`, not a `todo/tickets` slice

- It requires an architectural decision about what "authoritative" means for
  the legacy atomic lane when threads hold divergent `env` snapshots of the
  same name -- not a local code fix.
- It is a **pre-existing** flaw in the general `shared_vars` blanket-reconcile
  design (last-write-wins by bare name across the whole process), not
  something introduced by the sibling ticket's changes; it happens to be easy
  to trigger via a `cas`-then-reassign-then-cas sequence but is not specific
  to `cas`, cells, or the Seq/Array shape asymmetry.
- Reproducing it deterministically requires explicit `Channel`-based ordering
  control; it does not arise from the ordinary sequential-`await` pattern
  used throughout `t/cross-thread-shared-var-writeback-coherence.t`, which is
  why none of the existing pinned tests caught it and why the sibling
  ticket's regression tests (`t/atomic-cell-shape-refusal-symmetry.t`) also
  pass cleanly -- they don't exercise a stale-spawned, still-running thread.

## Affected files

- `src/runtime/builtins_atomic.rs` (`atomic_value_key_for_name`)
- `src/runtime/runtime_shared_vars.rs` (`reset_atomic_var_key`,
  `sync_shared_vars_to_env`)
- `src/runtime/builtins_atomic_cas.rs` (`builtin_cas_var`, whose 2-arg retry
  loop resolves `scalar_cell`/`value_key` once at entry and never re-checks)
