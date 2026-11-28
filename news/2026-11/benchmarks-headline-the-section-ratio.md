# Benchmarks headline the in-process section ratio, not the whole-script one

A whole-script benchmark row charges each interpreter its startup: raku's ~120-170 ms against
mutsu's ~4-8 ms. A light benchmark's ratio therefore sat near 1 however slow its operation was, and
rakudo was also charged its JIT warm-up.

Every `benchmarks/*.raku` except `bench-startup` (which measures startup on purpose) now runs its
workload as `workload()`, twice untimed and a third time timed, and prints
`bench-section-seconds:` — the convention the regex and grammar files already followed. The 27
files that lacked one gained it; their checksum lines are unchanged and identical under mutsu and
`raku`. Under `scripts/bench-det.sh` (`BENCH_DET=1`) the workload still runs once.

The bench CI therefore records `<name>@section` / `<name>@section+jit` rows for all of them, and the
trend page (`scripts/bench-visualize.py`) makes that series the card's headline: the chart, big
number, vs-rakudo / vs-rakupp medians, red/win colouring, the table's main columns and `--report`
all read it. The `foo@section` series is folded into the `foo` card instead of being a card of its
own. The whole-script series stays as a secondary one (a "whole script: rakudo X · rakupp Y" line,
dashed lines on the ratio charts, the table's last column), and `bench-startup` is shown as the
startup metric. A dotted line marks the commit where a card's section series starts, so the
missing earlier points read as a new series rather than a drop.

See `docs/benchmarks.md`.
