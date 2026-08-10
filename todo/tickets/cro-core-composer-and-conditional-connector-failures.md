# `Cro::Core` `composer.rakutest` / `connection-conditional.rakutest` each fail one subtest

## Affected tests

- `t/composer.rakutest` test 134 "Correct message, implying correct options
  passing and correct transforms" (133/134 pass).
- `t/connection-conditional.rakutest` test 23 "Conditional correctly evaluated
  to default" (22/23 pass).

Both are in the vendored `Cro::Core` suite driven via
`bash tmp/cro-suite-run.sh core` (see `handoff-cro-next-steps` memory /
`docs/batteries/web-framework.md`).

## Status

Confirmed pre-existing (not a regression): reproduces identically on
`f552e76aa` (main, before the `supply { done }` desugar fix in
`supply-done-in-method-supply-block-escapes-as-cx-return.md`) and after it.
Not investigated further — root cause unknown. Both suites (`core`/`http`)
were otherwise fully green or at their previously-recorded pass counts per
`handoff-cro-next-steps` memory (2026-08-09), so this is two isolated
pre-existing failures, not a suite-wide regression.

## Next step

Repro each subtest in isolation (extract the relevant `class`/`supply`
snippet to `tmp/`), compare against `raku`, and file proper root-cause
findings once diagnosed.
