# `Q:to/…/` wrongly honours a `\qq[…]` escape

`Q` is the *no-escapes, no-interpolation* quoting form: inside `Q`, a `\qq[…]`
sequence is literal text. mutsu processes it as an interpolation escape, the way
it correctly does for `q` and `qq`.

## Repro

```raku
my $name = 'world';
say Q:to/END/;
raw \qq[$name] here
END
```

```
raku:   raw \qq[$name] here
mutsu:  raw world here
```

The `q:to/…/` and inline `q[… \qq[$name] …]` forms are handled correctly — only
the `Q` family is wrong. `Q[…]`, `Q{…}` and `Q:to/…/` should all be affected;
the heredoc is just the shortest demonstration.

## Why it matters

`Q:to/…/` with a `\qq[…]`-looking body is exactly what a template engine emits
when it generates Raku source, so this is not a curiosity. `Template6`'s
`Parser.compile` builds its generated program out of nested `q:to`/`Q:to`
heredocs with `\qq[…]` interpolation
(`lib/Template6/Parser.rakumod`, the `compile` method) — see
`todo/deep/template-engines-blocked-on-mutsu.md`.

## Affected area

The quote-construct escape handling — whichever layer decides that `\qq[`
starts a nested interpolation needs to be gated on the construct being a
`q`/`qq` family form, not `Q`. Adverbs on `Q` (`Q:qq[…]`) do re-enable
interpolation and must keep working.
