# `(next LABEL)` in expression position keeps its loop label

`Rakudo-Type-Introspection` walks a stash with

```raku
LEVEL:
for <v6c v6d v6e> -> $core {
    ...
    for @parts -> $part {
        $WHO{$part}:exists
          ?? ($WHO := $WHO{$part}.WHO)
          !! (next LEVEL)
    }
}
```

and mutsu could not load the module: the labeled loop failed with this #7988
cluster's generic `Confused. expected statement: ...` message.

The statement forms (`next LABEL;`, `last LABEL if ...`) have always read the
label, and so did `... and next LABEL`, whose right-hand side is parsed as a
statement. But `next` / `last` / `redo` met in *expression* position (inside
parentheses, a ternary branch) are parsed by the primary-term parser, which
built an unlabeled `Expr::ControlFlow` and left the label behind as a stray
bareword. The compiler already emitted `OpCode::Next(label)` etc. for a labeled
`Expr::ControlFlow`; only the parser dropped it. The three keyword arms now read
an optional label with the same rule the statement forms use
(`is_loop_label_name`: an all-caps name or a declared loop label).

Pinned by `t/control/loop-label-in-expression-control.t`.
Rakudo-Type-Introspection's module now loads; its tests next stop on the
unimplemented `nqp::objectid` op, a separate gap.
