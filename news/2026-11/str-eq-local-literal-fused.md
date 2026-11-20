# `$ch eq ' '` is one op that compares in place

A tokenizer predicate is a chain of `$ch eq ' '`. Each compare was three
ops:

- `GetLocal` cloned the slot's `Str` onto the stack;
- `LoadConst` cloned the literal;
- `StrEq` popped both, compared them and dropped them.

The comparison itself is a length test and a `memcmp`. The clones, the drops
and the two extra dispatches cost more than that.

`$local eq 'literal'` and `$local ne 'literal'` now compile to one
`StrEqLocalConst` op (ADR-18357 D1). It borrows the slot's `Str` and the
constant where they live. It takes that narrow arm only when three things
hold:

- reading the slot is exactly what `GetLocal`'s own fast path would push;
- the slot holds a plain `Str`;
- no user candidate of the operator is in scope.

Otherwise it runs the three ops it replaces, through their own handlers. The
compiler fuses only a left operand that compiled to a single `GetLocal`, and
only outside every lexical-import scope.

Callgrind slopes on one profiling build, narrow forms on against off:

- a loop calling `is-space($c)` (five such compares) went from 10,193 to
  8,114 instructions per iteration, against 9,321 before this op;
- MCP::Server::Tool::Web's html-to-text, with all narrow forms on against
  off, is −0.49%.
