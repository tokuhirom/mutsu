# `say([+](1,2) + 1)`: an infix after a functional reduction inside call parens

A functional-form reduction could not be followed by an infix when it was an
argument inside call parentheses:

```raku
say [+](1,2) + 1;     # 4 -- worked
say([+](1,2) + 1);    # Confused. expected statement: expected expression statement or ')'
```

Statistics::Distributions and Data::Generators both contain the same
standard-deviation helper, `sqrt([+]((@x X- mean(@x)) X** 2) / @x.elems)`,
so neither could run its random-variate tests. This is one of the parse gaps
split out of the #7988 cluster (#9328).

`parse_call_arg_list` parses each parenthesized-call argument. It tried
`reduction_call_style_expr` first and, when that matched, took the bare
`[+](...)` as the whole argument. The next thing it expected was `,` or `)`,
so the `+ 1` that followed was a parse error. The statement-level argument
parser in `stmt/args.rs` already had these the other way around (full
expression first, call-style reduction only as a fallback), which is why the
listop form worked.

The call-argument parser now also runs the full expression parser whenever the
call-style reduction matches, and keeps whichever of the two reads further. A
reduction that really does end the argument (`f([+](1,2))`,
`f([\+](1,2,3), 9)`) parses exactly as before. Pinned by
`t/lang/operators/reduction-call-form-in-call-args.t`.
