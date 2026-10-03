# `Routine.prec` answers an operator's precedence hash

`&infix:<+>.prec` answered a `&<composed-method:prec>` Sub. It now answers
Rakudo's hash, `{assoc => left, dba => additive, prec => t=}` (#11324).

The built-in operators' hashes are a table generated from Rakudo 2026.09
(`src/op_prec/`), flags included (`iffy`, `diffy`, `thunky`, ...). A user
operator starts from its category's default (`default-infix` at `t=`), and its
traits change it as Rakudo's do: `is equiv(&op)` copies `&op`'s hash,
`is tighter`/`is looser` insert `@`/`:` before the level's `=` and reset
`assoc` to `left`, and `is assoc` overrides the associativity whatever order
it is written in. The parser computes the hash where it reads the
declaration, so a lexical operator declared relative to another user operator
sees that operator's hash, and hands it to the routine as a trait. A `multi`
candidate without traits takes the one its operator was declared with.

When an operator carries two precedence traits, the last one now wins, for
parsing as well as for `.prec`, as in Rakudo; the parenthesized form used to
keep the first.
