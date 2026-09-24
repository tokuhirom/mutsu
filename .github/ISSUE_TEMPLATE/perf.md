---
name: Performance finding — correct but slow
about: mutsu produces the right answer too slowly; the next step is profiling.
title: ''
labels: 'todo:perf'
assignees: ''
---

<!--
Use this ONLY when the next step is measurement. A perf-flavoured finding that
also fixes a genuinely wrong answer belongs under the ticket or deep template --
correctness ranks above speed, and filing it here hides a real bug behind a
benchmark. See docs/issue-workflow.md.

Its implementation agent runs SOLO: parallel perf agents on one box produce
measurements that drift and never converge.

The Goal section is required: the issue closes when it is met, and not
before. See "A todo:perf issue states its goal" in docs/issue-workflow.md.
-->

## Goal (close condition)

<!-- One metric, where it is read, and the threshold. Pick the shape that
     matches why you are filing:
     - slower than rakudo: "`<bench>+jit` row in bench-history.tsv at or under
       Nx rakudo" (or "faster than rakudo");
     - a regression: "`<bench>` row back within 3% of <baseline commit>",
       naming the commit that regressed it;
     - wrong complexity: "<operation> is O(<target>) per call, shown by
       scripts/<family>-complexity-check.sh" -- a constant-factor win does
       not meet this. -->

## Benchmark

<!-- The script or `benchmarks/` file that shows it, and how to run it. -->

## Measurement

<!-- Numbers, and where they came from. Numbers that end up in a repository
     document must come from the bench CI (`bench-history.tsv` on the
     `bench-data` branch), citing the main commit hash of the row; local runs
     are fine for the investigation itself but drift with thermals and binary
     layout. -->

| | mutsu | raku | ratio |
| --- | --- | --- | --- |
| | | | |

## Where the time goes

<!-- Profiler/counter evidence. `MUTSU_VM_STATS=1` counters are deterministic
     and identical in debug and release, so iterate against them with the debug
     build; `alloc_scope!` + the `alloc-stats` feature counts allocations
     exactly; callgrind works in this container where perf does not. -->

## Why it is not a quick fix
