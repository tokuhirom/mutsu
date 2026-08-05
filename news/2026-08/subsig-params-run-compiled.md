# Sub-signature params run compiled (ADR-0019 C6e-2b)

The OTF/plan-bytecode gate (`def_module_single_sig_body_ok_ignoring_state`)
no longer excludes routines with sub-signature (destructuring) parameters —
`sub group-of (Pair (Int:D :key($plan), ...))` and friends now run through
the shared compiled entry instead of the C6d-5 interpreter arm. This was the
smallest of the three gate-rejected shape classes measured for C6e-2 (15
interpreter-arm hits across `t/` + the roast whitelist, vs ~1,050 sigilless
and ~2,760 `start`-body).

Measured before changing anything: with the exclusion experimentally lifted,
the full `t/` suite (2,894 files) and all 37 roast files exercising
`Test::Util`'s `group-of` (the dominant destructuring consumer) showed zero
real regressions — the only diffs were a non-whitelisted file that fails
identically on the baseline and one debug-binary timeout under `-j4` load
that passes solo. The mechanism explains why: parameter binding runs through
the shared `bind_function_args_values` on both arms, and destructured
elements bind read-only (`MarkSigillessReadonly` for `(\i, \j)` shapes), so
the historical exclusion reason — the sigilless caller-alias writeback fixed
in C6e-2a — never applied to sub-signatures in the first place.

With C6e-2a and C6e-2b landed, parameter *shapes* no longer gate compilation
at all; the param predicate is down to the NativeCall marshalling traits
(`is encoded(...)`). The remaining gate-rejected class is `start`-containing
bodies (C6e-2c, the recursive-start param-capture problem) — tracked in
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`. Pinned
by `t/subsig-param-compiled.t`, including the nested `group-of` Pair shape
and an EVAL-boundary call.
