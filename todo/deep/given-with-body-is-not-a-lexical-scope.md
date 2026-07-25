# A `given` / `with` block is not a lexical scope for `my`

Found 2026-07-25 while fixing the `&`-sigil pointy parameter (#5405). Moved out
of PLAN.md §8.22 when discovered findings became per-file `todo/` entries.

## Repro

```raku
my $z = 1;
given 1 { my $z = 5; say $z }   # 5 in both
say $z;                          # raku: 1     mutsu: 5

my $x = 1;
given 5 -> $x { }
say $x;                          # raku: 1     mutsu: 5
```

## Root cause

A `my` declaration inside a `given` / `with` body writes through to the
enclosing scope instead of shadowing there, so both an explicit `my` and the
pointy-parameter head statement leak. Sigil-blind — `$`, `@`, `%` and `&` all
leak the same way. A bare `{ … }` block scopes correctly, so it is the
`given`/`with` body specifically that is not compiled as its own scope.

## Affected files

- `src/parser/stmt/control.rs` — `pointy_topic_bind`, which builds the
  parameter head statement prepended to the body.
- `src/parser/stmt/control/given_when.rs`, `src/parser/stmt/control/with_stmt.rs`
  — where that head statement is inserted.
- The compiler's block-scope handling for `Stmt::Given`.

## Why it is large

`given` bodies are shared with `when` / `default` and with the
statement-modifier forms, and the topic (`$_`) binding deliberately reaches the
enclosing frame — the scope boundary has to admit `$_` while excluding ordinary
`my` declarations. Getting that wrong in either direction breaks a lot of
working code.

## Impact

Silent clobbering of a same-named outer lexical, which is hard to spot precisely
because the block's own behaviour is correct. Not currently known to break a
dist, but it is a real divergence and the shape (`my $x` inside `given`) is
common.

The pin `t/pointy-code-param.t` deliberately stops short of asserting that a
`given ... -> &f` alias stops shadowing at the closing brace, because of this
gap; extend it when this is fixed.
