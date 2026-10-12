# A closure sees a lexical written by EVAL or a symbolic store

```raku
my $x = 5; my &g = { $x };
EVAL q[$x = 9];
say g();   # 9 -- was 5
```

A frame that can write one of its lexicals by a runtime name -- `EVAL`, a
symbolic store (`::('$x') = 9`, `$::($n) = 9`), `MY::<$x> = 9`, or
`nqp::bindlex` -- now keeps every plain scalar it declares in a shared cell,
including when the writer is a closure nested in the frame. Before, only
`nqp::bindlex` was covered, so a closure that captured `$x` before an
`EVAL` or symbolic write kept the old value (#12753).
