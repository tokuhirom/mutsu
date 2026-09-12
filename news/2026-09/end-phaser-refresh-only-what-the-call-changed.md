# The END-phaser refresh stops re-proving that nothing changed

Next slice of [#7565](https://github.com/tokuhirom/mutsu/issues/7565) ("`use Test`
taxes every hot loop in the file that loads it"). The previous four slices
(#7656, #7707, #7964, #8019) worked the closure *capture*: what goes into it and
what the filter walks to build it. This one is about what happens on the way
back out.

## Where the tax actually sits now

A per-function instruction-slope profile of the ticket's loop, taken against
`main` at `f4b55d7`, puts the biggest single term somewhere none of the earlier
notes looked: `Env::get_sym` is called **32 times per loop iteration from
`call_compiled_closure_in_unit`**, for 2 053 instructions of self cost, and
every one of those calls is part of the END-phaser refresh at the end of the
closure-return path. It is pure tax — the same loop without `use Test`
registers no `END` at all, so the refresh never runs and the term is zero.

The refresh exists to keep an `END` phaser's captured copy of a lexical current:
the phaser holds a *copy* of the scope it closed over, so when a closure mutates
a captured lexical whose declaring frame dies before exit, that copy is the
phaser's only surviving binding. Two things made it far more expensive than that
purpose needs:

1. **It iterated the whole capture.** The capture's width is set by the
   *creating scope*, not by the closure, so `use Test` alone made it ~35 names
   per return — and a reflective program's whole-env snapshot makes it hundreds.
2. **It re-stored every one of them**, whether or not the value had changed.
   Upstream `Test.rakumod` registers an `END` (its plan check), and the names a
   wide import list leaves in a capture are dominated by things no call ever
   rebinds: the ~19 built-in dynamics, `Any`, `?FILE`, the topic.

## Two changes

**Skip a write whose value is already there.** `update_end_phaser_envs_for_keys`
now compares by binding identity (`Value::same_binding`, an O(1) word compare)
and returns early instead of re-storing the very `Value` the phaser env already
holds. `same_binding` is the right test and a deep `==` would not be: it walks
container contents, and a container mutated in place keeps its binding — which
is precisely the case where the phaser's entry *is* that same container and
already sees the mutation.

**Consider only the names the call could have changed.** Every caller-visible
write a closure call makes passes through that frame's own overlay tier — the
closure-return writeback loop is an iteration of exactly that tier, and a value
that never reached it cannot have reached the restored caller env either. So the
overlay's own keys, intersected with the capture, bound the refresh: after a bare
`use Test` that is about 6 names instead of ~35. Three things widen it back:
`cc.free_var_syms` is added unconditionally (a slot-authoritative write to a free
variable need not have mirrored into `env` at all); a by-name write whose target
only exists at run time (an `EVAL`'d `$a = 32`, merged from a side channel by
`propagate_pending_caller_writes`) and a resume-safe `CONTROL` handler's write
into an ancestor frame both fall back to the whole capture. So does a frame whose
env is not a scoped overlay, where `keys()` is the whole env and the narrowing
would cost more than it saves.

The runtime overlay is the right source rather than any compile-time set: a write
made by a routine *called from* the closure body is named by no free-variable set
of the closure, and reaches the caller only through that routine's own writeback
into this frame — which is to say, through this overlay. `t/routines/closure/end-phaser-closure-capture-refresh.t`
pins that case along with five others.

## Numbers

Warm-run callgrind instruction slopes between 6 000 and 14 000 iterations,
`MUTSU_JIT=off MUTSU_GC=off`, release, on two shapes of the ticket's loop: the
original (a leaf closure, which skips the caller-writeback scan) and a variant
whose closure makes a call.

| | base | after | |
| --- | --- | --- | --- |
| leaf loop, no `use Test` (the floor) | 75 954 | 76 007 | |
| leaf loop `+ use Test` | 96 926 | 88 009 | -9.2% |
| **its tax** | **20 972** | **12 002** | **-42.8%** |
| calling loop, no `use Test` | 82 595 | 82 618 | |
| calling loop `+ use Test` | 106 196 | 97 181 | -8.5% |
| **its tax** | **23 601** | **14 563** | **-38.3%** |

Ablated in a separate gated build (its baseline carries ~1.3k of gate overhead,
so these rank the two changes rather than restate the table above): the
equal-write skip alone is 29% of the tax, and deleting the refresh outright —
the ceiling — is 48%. Shipping both reaches **88% of that ceiling** while
keeping the mechanism.

## What is left

Revised from the list in #8019's note:

1. **The no-op capture merge.** Unchanged and now the largest remaining term:
   gating it out entirely is worth 4 821 instructions per iteration on the leaf
   loop (measured here, higher than the 2 635 the previous note reported because
   the profile has moved). Still the design change #8019 described — chaining the
   capture *under* the caller as a fallback tier rather than merging it key by
   key — and still an ADR rather than a perf slice. A cheap identity token for
   "the caller chain is the chain this capture was filtered out of" is the
   tempting shortcut and it is **not** cheap: the only sound token pins the
   creating frame's overlay `Arc`, which forces a copy-on-write clone of that
   overlay on the very next write to it — and in this loop that write is the
   `my &return = ...` that stores the closure itself.
2. **`skip_env_write`'s process-global latch** (`vm_var_assign_set_local.rs`),
   with the `EVAL`-scope question #8019's note spelled out. Gates 3.
3. **The `filtered_flat` walk**, still the largest self cost in the program
   (5 024 instructions per iteration on the leaf loop with `use Test`, against
   2 121 without). Needs 2 first.
4. **The ~20 built-in dynamics** — floor, not tax.

One measurement note for whoever takes these: the equal-write skip means a
profile of the refresh now under-reports its *potential*, because the writes it
used to make were the cheap half. Measure a change to it against the numbers in
the table above, not against any earlier note's.
