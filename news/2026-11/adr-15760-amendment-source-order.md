# ADR-15760 Amendment 1: where the early classification really comes from

The first attempt at ADR-15760 slice 1 (source-order classification in the frontend) changed none of
the rows of #15791, because the frontend's whole-unit scan is one of five sources. The ADR now records
all five, measures the rows against raku, and splits slice 1: one post-declared-type check for every
unit, order-aware term and type excuses in the existing undeclared-routine scan, the `.AST` view in
source order, and the carried-uppercase-bareword case moved to slice 2. Proposed; three questions for
the maintainer are listed in §6.5.
