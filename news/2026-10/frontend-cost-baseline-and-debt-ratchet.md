# The frontend cost has a frozen baseline and the frontend debt has a ratchet

ADR-15334 slice 0 adds the two measurements every later Stage 2 slice reports.

`scripts/frontend-cost.sh` measures the callgrind instruction count of the frontend (parse, build and
lower, from the entry of `parse_compilation_unit_of` to the return of the lowered statements) for five
cases, and compares it with `scripts/frontend-cost-baseline.txt`, recorded at `1340492a`, the parent of
the commit that made the RakuAST round trip the default. `make check-frontend-debt` fails when the
`SyntheticBlock` references or the distinct `"__..."` literals in `src/parser`, or the lines of
`src/rakuast/convert.rs`, rise above the ceilings in `scripts/frontend-debt.d/`.

The first measurement of `main` is 102-123% of the baseline, with three of five cases above the 110%
bound of ADR-15334 section 3.2. The ADR amendment that reconciles the bound is a separate proposal.
