# A session of slices that went nowhere

One session, eight merged performance PRs, every one of them sound, measured
and general. `JSON::Fast` went from about 63x rakudo to about 59x.

This entry is about why, and about the rule added to
[`.agents/skills/perf-tuning/SKILL.md`](../../.agents/skills/perf-tuning/SKILL.md)
§5a so the next session does not repeat it.

## What happened

The goal was "get mutsu near rakudo". The work started from
[#8830](https://github.com/tokuhirom/mutsu/issues/8830) — runtime string-keyed
name resolution — and proceeded by profiling, fixing the top entry, and
repeating.

| landed | measured |
| --- | ---: |
| six name-resolution slices (#8843, #8851, #8854, #8859/#8861, #8868, #8874) | 2,975,774,664 → 1,843,258,057 Ir on the benchmark |
| #8876, `nqp::*_i` inlined into Tier B | -44% on a numeric loop, **-0.1%** on `JSON::Fast` |
| #8879, the `DESTROY` sweep skipped when no user `DESTROY` exists | -3.9% on a loop, **0%** on `JSON::Fast` |
| #8886, the dispatch chain's single most expensive probe, removed outright | **3.1%** of a method call |
| #8890, `as_str` instead of `resolve` on the native dispatch path | **393** instructions of 22,528 |
| pure-tag gating of the `native_method_0arg` prologue | **1.17%**, reverted |

The analysis produced alongside them was, each time, correct — and each time
ignored by the next commit:

* After the six name-resolution slices: *eliminating all runtime name/type
  resolution is 1.28x; the last slice removed **65.8% of the profile's single
  largest entry** for **4.3%** overall.*
* After #8886, in its own PR body: *"there is no ordering of probe removals that
  reaches a method call worth calling fast, because the chain is the design."*
  The next two commits removed another cost from the chain.

## The arithmetic that was never done

59x to 10x is **6x**. A 3% slice contributes 1.03. Sixty of them, with no
interaction, would be 5.9.

That line takes one minute to write and would have ruled out the entire
sequence on day one. It is now §5a's first rule: **state the required multiple
before the first slice, and check that the plan's slices multiply to it.**

## Why it was seductive

Each step is individually defensible. Profile, find a real top entry, fix it
soundly, measure it, land it green. Nothing in that loop is wrong. What is
wrong is that it has no exit condition, and it *feels* like progress the whole
way because every PR is measured and merged.

The tell, stated in §5a: **your own write-up keeps concluding "the structure is
the cost" while your next commit keeps not changing the structure.** That
pattern was visible in this session's own PR bodies and was not acted on.

## What the measurements were actually worth

The slices did not move the goal. The measurements did, and they are the
session's real output:

* **The gap is not a uniform per-operation tax.** Arithmetic, strings and
  containers already run at 4-10x rakudo — no type specialization needed.
  (`benchmarks/micro/primitive-ops.py`, added in #8882.)
* **It is the call.** 22,528 instructions for a user-class method call, 8,314
  for `@a.elems`, against rakudo's ~30 cycles.
* **And the call is not one slow mechanism.** `@a.elems` is *fifteen* layers of
  100-900 instructions, **each already carrying a cache from a previous
  campaign** — `native_lever_a_user_override_sym` is address-keyed to avoid an
  intern, cites two campaigns in its doc comment, and still costs 87
  instructions on a *hit*, twice per call; `vm_call_method_compiled_cache.rs`
  is 849 lines of caching; `Symbol::intern`, `MetaNs::key`,
  `type_meta_key_cache` and `capture_candidates` all memoize.

That last point is the one that closes the argument, and it became §5a's second
rule: **an existing per-layer cache is evidence against adding another one.**
Fifteen layers of cache lookups is what "make each layer cheap" converges to.
What MoarVM does instead is resolve the call site once and remember the
answer — a cache *in front of* the layers.

## Where that leaves the issues

[#8880](https://github.com/tokuhirom/mutsu/issues/8880) and
[#8888](https://github.com/tokuhirom/mutsu/issues/8888) were filed as two
problems, one for user-class receivers and one for builtin ones. They are one:
a per-call-site cache saying "at this call site, a receiver of type T resolves
to *this* handler", keyed on a class `Symbol` for the first and a NaN-box
`Kind` for the second, sharing its storage and its invalidation. Both issues
carry the correction.

[#8877](https://github.com/tokuhirom/mutsu/issues/8877) records the one finding
from the name-resolution campaign worth keeping: a typed lexical's declared
constraint is an env probe **per store**, and belongs on `BindingDesc`
(ADR-0097) rather than being memoized.

## What the slices are still worth

They are not withdrawn and should not be. Every one is a real, sound, general
improvement that makes some program faster and none slower, and two of them —
the `DESTROY` sweep and the `as_str` allocations — apply to essentially every
Raku program. A codebase is better for having them.

They are simply not a path to the goal, and the mistake was in believing that a
sequence of them could be one without ever checking the multiple.
