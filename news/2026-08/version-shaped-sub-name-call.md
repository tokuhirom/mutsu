# Version-shaped routine names can be called

Routine names consisting of `v` followed by digits, such as `v1`, `v2`, and
`v10`, now parse as routine calls when immediately followed by parentheses.
Previously the version-literal parser claimed the name first, so `v1(5)` tried
to invoke a `Version` value and failed with a misleading `CALL-ME` error.

The version parser now yields to the identifier parser for this call-shaped
syntax. Declared routines are invoked normally, undeclared names receive the
standard undeclared-routine diagnostic, and bare spellings such as `v1` remain
Version literals.
