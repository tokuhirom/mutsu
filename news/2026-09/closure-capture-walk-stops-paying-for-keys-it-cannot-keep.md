# The closure-capture walk stops paying for the keys it cannot keep

`Interpreter::capture_closure_env` builds a closure's captured env with
`Env::filtered_flat`, which walks every key visible from the creating scope and
keeps the ones the closure may name. [#7707](https://github.com/tokuhirom/mutsu/issues/7565)
and [#7964](https://github.com/tokuhirom/mutsu/issues/7565) taught that filter to
*reject* two whole families of metadata key a wide import list leaves in the
importer's scope. They did not stop the walk from *visiting* them, and visiting
is most of what the walk costs.

Measured on the `use Test` loop from [#7565](https://github.com/tokuhirom/mutsu/issues/7565),
a closure created inside a sub walks **95 visible keys and keeps 31**. The 64 it
rejects are almost entirely the module's own bookkeeping:

| family | count | why it is rejected |
| --- | --- | --- |
| `__mutsu_callable_id::<pkg>::<name>` | 49 | routine-registration marker; every consumer reads it from the live env at call time |
| `__mutsu_type::<name>` | 11 | shadow metadata whose subject is not captured |
| plain user lexicals that are not upvalues | 3 | not free variables of this closure |
| kept | 31 | the built-in dynamics, `Any`, `?FILE`, the topic, … |

Two changes, both pure to the filter's existing verdicts.

## `filtered_flat` no longer removes from an empty map

`collect` walks the chain outermost tier first and, for every key the filter
rejects, calls `out.remove(k)` so that a nearer tier's rejection suppresses an
outer tier's kept entry. On the **outermost** tier that remove is a guaranteed
miss: `out` is still empty, there is no outer tier whose entry could need
suppressing, and a map yields each of its own keys once. It still cost a hash and
a probe per rejected key — on the widest tier of the chain, the one a wide import
list inflates. All 62 removes this loop performed per closure creation landed on
that one tier.

`collect` now carries an `outermost` flag and skips the suppressing remove (and
the tombstone sweep, unreachable for the same reason) while it holds. Pinned by
`env::tests::filtered_flat_outermost_tier_rejections_do_not_leak`.

## The filter answers the bulk of the walk with one flags word

The per-key predicate asked its cheapest questions last. It now tests the two
unconditional rejects — `ATTR_TWIGIL_ENV_KEY` and `CALLABLE_ID_META` — as a
single fused mask over the memoized `Symbol::flags` word, before the
`__mutsu_callable_type` symbol compare; and it answers a plain user lexical with
`free`'s membership alone, since the `own_locals` probe cannot change that
verdict and `free` is empty for most closure literals. A system name short-cuts
on `own_locals.is_empty()`. Every one of the old expression's verdicts is
preserved — it is the same boolean, reassociated so that the common key exits
first.

## Numbers

Warm-run callgrind instruction slopes between 6 000 and 14 000 iterations,
`MUTSU_JIT=off MUTSU_GC=off`, release, on two shapes of the ticket's loop: the
original (a leaf closure, which skips the caller-writeback scan) and a variant
whose closure makes a call (which takes it).

| | base | after | |
| --- | --- | --- | --- |
| leaf loop, no `use Test` (the floor) | 76 282 | 75 905 | |
| leaf loop `+ use Test` | 98 496 | 95 431 | -3.1% |
| **its tax** | **22 214** | **19 526** | **-12.1%** |
| calling loop, no `use Test` | 83 117 | 82 668 | |
| calling loop `+ use Test` | 108 550 | 105 427 | -2.9% |
| **its tax** | **25 433** | **22 759** | **-10.5%** |

Attributed by ablation on the leaf loop: the outermost-tier remove skip is
-2 272 of the tax, the predicate reorder a further -416. `Env::filtered_flat` was
the single largest self cost in the whole program (5.4%) before this and
`RawTable::remove_entry` (2.1%) has left the profile's delta entirely.

## What the measurement turned up for whoever takes this next

**The captured-env merge at closure entry inserts nothing at all, and costs 2 635
instructions per iteration to find that out.** `call_compiled_closure_in_unit`
merges the capture with `entry_or_insert_sym_with` per entry — don't overwrite,
so the live caller binding wins, which is what dynamics need. On this loop
`entry_or_insert_sym_with` runs 31 times per call, its `contains_key_sym` runs 31
times, and `insert_sym` is reached **zero** times: every captured name is already
visible through the caller's frame chain, because the capture was filtered out of
that very chain moments earlier. At 72 instructions per `contains_key_sym` that
is 2.7% of the loop spent proving a no-op.

Making it cheap per key is not the fix; the shape is. The precedence the merge
implements by hand — caller chain wins, except for an explicit overwrite list
(`ContainerRef` cells, `self`, a block's topic and `$!`, the authoritative and
owned capture lists) — is exactly what `overlay -> caller chain -> capture` would
give for free if the capture were chained *under* the caller as a fallback tier
instead of merged key by key. That needs `Env` to be able to extend a chain at
its tail (or a fallback map consulted between the chain and `GLOBAL_BASE`), which
is a design change worth an ADR rather than a perf slice.

Also measured and **not** worth redoing blind:

- **Hoisting the caller-writeback scan's "captured name, unchanged since
  capture" `continue` to the top of its loop is a regression**, built and
  measured at +614 instructions per iteration on the calling loop's floor. The
  reasoning that suggests it ("the merge leaves captured names in this overlay,
  so the scan sees ~30 of them") is false precisely because of the finding above:
  the merge leaves *nothing* in the overlay, so the hoisted test fires for no key
  and only adds a probe per key to the ~6 that are really there.
- The remaining tax is still capture plus merge, now over 95 walked keys and 31
  merged ones. The two items the previous note left stand unchanged: the ~20
  built-in dynamics every capture carries (they are 19 of the 31, and they are
  floor, not tax — dropping them needs a way to tell an interpreter's own
  `init_io_environment` default from a user `my $*OUT = …` redirection, which a
  key-pure filter cannot see), and the per-tier memo of the walk, which cannot
  arm while the process-global reflective latch keeps `SetLocal` mirroring every
  mainline store into the env (`vm_var_assign_set_local.rs`'s `skip_env_write`
  still consults the global flag, not #7707's per-chunk one). That latch is the
  ticket's own "worse of the two" and is still unfixed for stores.
