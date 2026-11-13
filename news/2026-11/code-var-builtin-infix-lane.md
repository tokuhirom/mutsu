# `cmp($a, $b)` through a `:&cmp = &[cmp]` parameter runs the operator directly

A routine taking its comparator as a code parameter defaulted to a built-in
operator (`sub finds(\haystack, $needle, :&cmp = &[cmp])`, Array::Sorted::Util)
calls it on every step of its search. With the parameter's slot known to the
compiler (#17611), a two-argument call whose slot holds a plain built-in
infix routine now goes straight to that operator's body: the same
`try_plain_infix_routine_value` answer the general value call reaches, minus
collecting the arguments into a vector and re-deriving the call site's shape.
A user candidate or `proto` for the operator, a CALL-ME role composed into the
routine, a pair argument and junction operands all still take the full path.
About 1.3k fewer instructions per call (17.3k per step of a `cmp`/`AT-POS`
scan loop, was 18.6k).
