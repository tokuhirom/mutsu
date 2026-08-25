# `my $b := 1; $b.VAR.^name` is `Int` again — a bind to a literal has no container

Ordinary assignment to a `$`-sigiled `my` variable creates a fresh `Scalar`
container; `:=` instead makes the name refer directly to whatever is on the
right-hand side, with no container in between. So `.VAR` on a bound variable
returns the bound value itself:

```
$ raku   -e 'my $a = 1; say $a.VAR.^name; my $b := 1; say $b.VAR.^name;'
Scalar
Int
```

mutsu answered `Scalar` twice: every `$`-sigiled `my` declaration was treated as
getting a container regardless of whether the initializer used `=` or `:=`.

The fix came out of the readonly-assignment taxonomy work
(`news/2026-08/readonly-assign-exception-taxonomy.md`), which is the same fact
approached from the other side. That work replaced the interpreter's
name-keyed readonly *set* with a map recording *why* each name is readonly:

* `ReadonlyKind::Alias` — a readonly binding that still owns a container (a
  non-`is rw` parameter, a `for @a -> $v` alias);
* `ReadonlyKind::Immutable` — a sigiled variable bound straight to an immutable
  value, with no container at all (`my $b := 1`, `my constant $E = 2`);
* `ReadonlyKind::ImmutableValue` — a name that *is* the value (a sigilless
  `constant PI` term).

`.VAR` now consults that kind: the two containerless kinds return the value
itself, everything else keeps returning the `Scalar` wrapper. That matches
Rakudo across the board — `my $b := 1` is `Int`, `my constant PI = 3.14` is
`Rat`, `my constant $E = 2` is `Int`, while a readonly parameter and a `for`-loop
alias both stay `Scalar` (they really do have containers, they just refuse
writes).

Pinned by the `.VAR` section of `t/readonly-assign-exception-taxonomy.t`, which
passes verbatim under both `raku` and `mutsu`.

One narrower gap remains and is tracked separately: a topic aliased to a literal
(`for 1,2 { $_.VAR.^name }`, `given 5 { $_.VAR.^name }`) still reports `Scalar`
where Rakudo says `Int` — see
`todo/tickets/topic-var-name-still-scalar-for-literal-alias.md`.
