# Deterministic benchmark series gain startup-free `@net` rows

`scripts/bench-det.sh` counted instructions and allocations for the whole process, so mutsu's own
startup was a constant floor under every file and hid how much a light benchmark's own work had
moved. It now also prints `<name>@net` and `<name>@net+jit` for each benchmark: the raw count minus
an empty script's count, measured in the same job and JIT mode. The raw series are unchanged.

The bench trend page shows them as the *instructions (net)* and *allocations (net)* metrics, and
`@net` rows are folded into the benchmark's card instead of becoming cards of their own. Subtracting
a measured baseline was chosen over callgrind's `--toggle-collect` because it needs no interpreter
function names and cannot silently collect nothing; the limits (parse/compile and module loading stay
in the net count) are in `docs/benchmarks.md`.
