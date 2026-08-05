# Sigilless scalar params run compiled (ADR-0019 C6e-2a)

The OTF/plan-bytecode gate (`def_module_single_sig_body_ok_ignoring_state`) no
longer excludes routines with sigilless scalar parameters (`sub f(\x)`), so
they run through the shared compiled entry instead of the C6d-5 interpreter
arm. Measured before changing anything (per the survey discipline): across the
whole `t/` suite the interpreter arm received 168 calls — 63 sigilless, 101
`start`-body, 1 sub-signature, 0 trait-based; across the roast whitelist,
3,677 — ~990 sigilless, 2,659 `start`-body, 14 sub-signature. An A/B run with
the gate experimentally widened showed exactly one failing shape out of all 19
sigilless-sub test files and the full `t/` suite: the EVAL-boundary
caller-alias writeback (`t/sigilless-params.t` test 3).

Two general fixes in the compiled return path (`vm_call_named_inner.rs`) made
the widening safe:

- **The alias-chain flush.** A compiled body writes a sigilless param through
  its local slot, and the caller-alias writes only reached the caller via the
  Slice F slot drain — which repairs *compiled* caller slots and runs after
  the caller-env merge, so an interpreter-frame caller (an EVAL body) read the
  stale pre-call value. The return path now flushes the param's final slot
  value through the `__mutsu_sigilless_alias::` chain (reusing
  `propagate_sigilless_alias_chain`) before the merge, mirroring the
  interpreter arm's `merge_sigilless_alias_writes`.
- **The name-collision re-apply.** The caller-env merge skips callee-local
  names, which silently dropped the writeback whenever the caller's variable
  had the same bare name as the parameter (`sub rts(\x) {...}; my $x = 1;
  rts($x)` — caller `$x` and param `x` share the env key `x`). The collected
  (target, value) pairs are re-applied to the restored env unconditionally
  after the merge, matching the interpreter arm's unconditional insert. This
  shape had no pre-existing test; `t/sigilless-param-compiled-writeback.t`
  now pins it alongside the EVAL, die-after-write, and sequential-call cases.

This is the first of the C6e-2 sub-slices
(`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`): the
remaining gate-rejected shapes are non-capture sub-signature params (15 hits
total) and `start`-containing bodies (the recursive-start param-capture
problem), still routed to the interpreter arm.
