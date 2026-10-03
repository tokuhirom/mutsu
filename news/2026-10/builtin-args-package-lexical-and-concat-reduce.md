# Builtins read a package-body lexical's value; `[~]` joins a lone list

SortUk sorts by the Ukrainian alphabet. Its `cmp_ch` asks
`$one & $two eq any $CHARSET`, where `$CHARSET` is a `constant` of the
`unit module`. Under mutsu the test was never true, so `ґ`, `є`, `і` and `ї`
were ordered by code point.

A routine reading a lexical that belongs to a class, module or compunit body
passes it to a call as a reference to that store's shared cell. That is what
an `is rw` parameter needs. The native builtin table stripped the reference
but kept the cell, and every handler saw the cell as one opaque item:

- `elems($m)` answered 1;
- `sort($m)` answered Nil;
- `any($m)` built a one-element junction.

The table's single entry point now reads through the cell, so all of these see
the list.

While there, `[~]` over one Positional now matches Rakudo. `[~] $x` with
`$x = (1, 2)` is `infix:<~>($x)`, whose `(@args)` candidate joins the list, so
the result is `"12"`, not `"1 2"`. The single-operand `[~]` rules now live in
their own file, `vm_reduction_concat_single.rs`.

SortUk's `t/01-basic.t` passes 4/4. Its `t/00-meta.t` needs `Test::META`,
which is not installed for Rakudo either.
