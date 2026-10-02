# A `&infix:<op>` parameter is the operator; a lone `proto sub infix:<op>` declares it

`proto sub infix:<precedes>($, $) {*}` on its own now declares the word
operator for parsing, as an `only` or `multi` sub always did, and records its
associativity and precedence traits. Before, only a candidate declaration
registered the name, so `$a precedes $b` after a bare proto died with "Two
terms in a row" at compile time.

Inside a routine or a parameterized role whose signature binds
`&infix:<precedes>`, the operator now resolves to that parameter. Role
parameters are now recorded with the other `&`-parameter shadow names, and a
builtin operator bound to the parameter (`f(&[<], 1, 2)`,
`class C does Heap[&[>]]`) counts as a lexical override and gets plain operand
values. This is the parameter shape the `BinaryHeap` distribution uses
(`role BinaryHeap[&infix:<precedes> = * cmp * == Less]`). Its heap order still
depends on #10530 (rebinding to a variable that is bound to an array element).
