# The bench CI now records the number #8673 is decided by

`bench-json-fast` has tracked JSON::Fast since #8288, but its ratio never showed
the gap the issues kept reporting: it times the whole script on a 2.4KB META6
document, so rakudo's ~0.15s startup floor is larger than the JSON work and the
row reads near 1x, while the `from-json` call itself is ~60x slower than
rakudo's. #8673 — now the single tracking issue for JSON::Fast decode speed,
with the goal set by the maintainer as *faster than rakudo* — needed a number
that could actually say whether that goal was met.

Two changes provide it:

- **`benchmarks/bench-json-fast-spdx.raku`** generates a synthetic SPDX license
  list (727 records, ~378K chars, the shape and size of the `licenses.json` that
  `License::SPDX` decodes on every `Test::META` `meta-ok()` call) and times one
  `from-json` of it from inside the process, printing
  `bench-section-seconds: <s>`. It is deliberately ~3s on release mutsu: the
  ratio widens with document size, because rakudo's JIT warms up on the parse
  loop and mutsu's cost per record stays flat, so a small document would
  under-report it.
- **`scripts/bench-ci.sh`** picks up that line and, for any benchmark that
  prints it, records two extra series, `<name>@section` and
  `<name>@section+jit`, whose raku column is rakudo's own in-process time. The
  `@section+jit` suffix order keeps `scripts/bench-visualize.py`'s `+jit` lane
  split working. Other benchmarks' rows are unchanged.

`scripts/bench-det.sh` now exports `BENCH_DET=1`, which the new benchmark reads
to shrink to 100 records under callgrind (~58s for both JIT lanes, 1.86G
instructions each), so the deterministic series stays affordable.

First local reading (4-core container, release build, 2 runs):

| row | mutsu | raku | ratio |
|---|---:|---:|---:|
| `bench-json-fast-spdx` (whole script) | 3.33s | 0.68s | 4.86 |
| `bench-json-fast-spdx@section` | 3.22s | 0.052s | **61.5** |
| `bench-json-fast-spdx@section+jit` | 3.11s | 0.052s | **59.5** |

The whole-script row still understates the gap by more than 10x; the section
rows are the ones to read.

The same change records the working rule for #8673 in
`.agents/skills/perf-tuning/SKILL.md` §5b: over twenty merged 1-9% slices moved
the ratio from ~77x to ~60x, so a change presented as progress on it has to say
concretely what is slow, how often it runs, what MoarVM does instead, and what
structural change removes the work — pruning, not shaving. And
`.agents/skills/ecosystem-dist-fix/SKILL.md` now tells ecosystem agents to add
their distribution to #8673 instead of filing another JSON::Fast perf issue
(#9061 was the latest duplicate).
