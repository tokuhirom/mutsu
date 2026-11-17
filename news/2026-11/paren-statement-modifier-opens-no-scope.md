# A parenthesized statement modifier opens no scope

`(EXPR given X)` and `(EXPR if COND)` used as a term ran their body as an inline
block, which pushed a lexical scope the source does not have. `OUTER::` from
the body therefore resolved one scope too close: `my $x = 1; say (OUTER::<$x>
given 1)` printed 1 where rakudo prints `Nil`. A modifier whose body is one
expression now compiles that expression in the enclosing scope; the `for`
modifier keeps running its statement as a block, as on rakudo. Fixes #17902.
