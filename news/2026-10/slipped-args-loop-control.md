# `last |c`, `next |c`, `redo |c`, `proceed |c` and `succeed |c` parse

The Spanish distribution wraps the loop-control words as `my &romper = sub (|c) { last |c };`, which
mutsu rejected with "Two terms in a row", so the module could not even load. Rakudo reads
`last |c` as the call `last(|c)`. The loop-control words now accept a slipped argument list: with an
empty capture they behave as the plain keyword. A non-empty capture would carry a `Label` value,
which mutsu does not model yet (labels are static names in the opcode), so it raises an explicit
error rather than dropping the argument; that and two neighbouring findings are tracked in #10554.
The Spanish `t/03-operators.t` file, the only one rakudo passes cleanly, now passes.
