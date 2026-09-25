# The `nqp::` loop forms yield a lazy Seq where their value is used

`nqp::while`, `nqp::until`, `nqp::repeat_while` and `nqp::repeat_until` always
compiled to a plain jump loop that discarded each body value and yielded `Nil`
([#9415](https://github.com/tokuhirom/mutsu/issues/9415)). That is correct where
the loop is sunk, and sunk is how ecosystem code uses them (CBOR::Simple, JSON::Fast,
`are`). Where the loop's value is used, rakudo yields a lazy `Seq` of the body
values instead:

```raku
use nqp;
my $i = 0; say nqp::while(nqp::islt_i($i, 3), $i++).raku;   # (0, 1, 2).Seq   (was Nil)
my $a = 0; my @a = nqp::while(nqp::islt_i($a, 3), $a++);     # [0 1 2]         (was [(Any)])
my $n = 0; my $s := nqp::while(nqp::islt_i($n, 3), $n++);
say $n;          # 0, because nothing has run yet
say $s.head(2);  # (0 1)
say $n;          # 2
```

In rakudo the loop yields `Nil` at a statement root and as the tail of a block
or routine (`sub f { ...; nqp::while(...) }` returns `Nil` after running the
loop). It is a `Seq` only as an operand: an argument, the right-hand side of an
assignment or binding, a list element, or an `nqp::if` branch.

## How

The bytecode compiler now tracks `expr_depth`, the number of `compile_expr`
frames between the expression being compiled and the nearest statement
boundary. `compile_stmt` starts a statement at depth 1, and `with_stmt_root`
resets a `Stmt::Expr` statement root, or a block or routine tail, to 0. So an
`nqp::` form compiled at depth 1 is sunk, and anything deeper is an operand.
Every operand of `nqp::stmts` is in sink position, the last one included. Rakudo
compiles a loop there as a void loop: `nqp::stmts(nqp::while(...))` runs
eagerly and yields null. JSON::Fast depends on this. Its `parse-obj` wraps its
`while (1) { ... return %result ... }` in an extra `nqp::stmts` ("this level is
needed for some reason"). A first cut that let the last operand inherit the
position of the enclosing `nqp::stmts` turned that loop into a lazy Seq.
`return` inside the Seq then could not leave the routine, and every JSON object
decoded wrong. The branches of `nqp::if` take the position of the `nqp::if`. A
sunk loop body is sunk too.

A sunk loop keeps the existing jump loop, so the hot path is unchanged. A loop
in value position is compiled as the AST
`gather { while COND { take BODY } }`, or as a `repeat` `Loop` for the
post-test forms. That is the same gather-backed lowering that a `(while ...)`
expression already used, so laziness and closure capture come from there.

TRIR had the same sink-only model. It now compiles the loop forms only in sink
position (`compile_expr_sink`), in tail position and as the last operand of
`nqp::stmts` (`compile_expr_tail`, where the loop is sunk and followed by
`Nil`). In any other position TRIR declines, and the bytecode path builds
the Seq.

## Known divergences

- A tail `nqp::if` whose branch is a loop yields `Nil` in mutsu. Rakudo yields a
  `Seq` there. Treating the branches as sunk keeps the statement-level
  `nqp::if(c, nqp::while(...))` idiom a jump loop.
- Rakudo yields null (`Mu` or `VMNull`) for `nqp::stmts(..., nqp::while(...))`.
  mutsu yields `Nil`.

Pinned by `t/vm/nqp-loop-value-context.t`.
