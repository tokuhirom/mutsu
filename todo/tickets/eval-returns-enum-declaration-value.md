# `EVAL` of a string whose first statement is an `enum` returns the enum, not the last statement

Found 2026-09-07 while writing `t/user-infix-op-candidate-ranking.t`, which needs
each row's declarations to live inside its own `EVAL` so candidate sets cannot
leak between rows.

## Repro

```raku
say EVAL(q|enum E <A B>; my $r = 41 + 1; $r|);
```

- `raku`: `42`
- `mutsu`: `Map.new((A => 0, B => 1))`

The enum declaration's own value wins over the block's last statement. It is not
about the value being computed — the body runs correctly, and

```raku
say EVAL(q|enum E <A B>; say "ran"; 0|);
```

prints `ran` and then `Map.new((A => 0, B => 1))` rather than `0`. Any `EVAL`
whose string *starts* with an `enum` declaration is affected; moving the enum
after another statement was not tested.

## Why it matters

It makes `EVAL` unusable as an isolation boundary for any test that needs to
declare an enum, which is exactly what per-row candidate-set isolation needs.
`t/user-infix-op-candidate-ranking.t` works around it by declaring
`enum E <A B>` at file scope and putting only the `multi` inside the `EVAL`.

## Where to look

The `enum` declaration presumably leaves its `Map` on the stack (or records
itself as the unit's result value) instead of being sunk, and the EVAL result is
read from there. Compare how a `class`/`sub` declaration inside an `EVAL`'d
string is sunk — those do not have the problem.
