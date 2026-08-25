# `unless COND -> $x { }` rejects the pointy-block parameter that `if`/`while`/`with` already accept

Originally filed as "`if`/`unless`/`while` block body rejects a pointy-block parameter".
**Re-measured on `main` @ `17139dd55` (2026-08-25): `if`, `while`, and `with` all accept the
pointy form now** — only `unless` is left, so the ticket is rescoped to it.

## Repro

```raku
$_ = 1;
unless 0 -> $_ { $_.say };
```

- `raku`: `0` (the pointy-block parameter binds the condition's value, shadowing the topic)
- `mutsu`: `===SORRY!=== Error while compiling ... Missing block ... at -e:1`

## Already working (do not regress these)

```
$ mutsu -e 'if 5 -> $x { say $x }'                  # 5   -- matches raku
$ mutsu -e 'my $i=3; while $i-- -> $x { say $x; last }'  # 3   -- matches raku
$ mutsu -e 'with 7 -> $x { say $x }'                # 7   -- matches raku
```

## Root cause

The parser accepts a `-> $param { ... }` signature form on the conditional block for `if`,
`while`, and `with`, but the `unless` arm still requires a plain `{ ... }` block. Since three
sibling constructs already do this, the fix is almost certainly to route `unless` through the
same block-parsing helper they use rather than to add a fourth bespoke path. Check `until` at
the same time — it is `while`'s negated twin and was not measured here.

Note the value that gets bound: `unless 0 -> $_ {...}` binds **`0`**, the condition's own
value, not its negation — so whatever helper is reused must pass the unnegated condition
result as the block argument.

## Affected files (starting point)

- `src/parser/` — the `unless` (and `until`) conditional-block parsing arm; diff it against
  the `if`/`while`/`with` arms that already accept a pointy block.
