# The slow-path debt is measured: a static ratchet and a dynamic survey

`make check-slow-path` counts `TODO: compile to bytecode` markers and the
`call_method_with_values(` call sites under `src/vm/`, per file, and fails on a new
one (a count may fall without an edit, so shrinking PRs never conflict on the baseline).
`scripts/slow-path-survey.sh` sums the distinct `Class.method` shapes that still enter
`run_instance_method` over `t/` and the roast whitelist: 132 shapes, 380 calls on
2026-10-24. PLAN.md section 0 carries the retirement project (#11510).
