# `my $b := 1; $b.VAR.^name` should be `Int` (no container), mutsu gives `Scalar`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Scalar.rakudoc:43`).

## Repro

```raku
my $a = 1;
say $a.VAR.^name; # Scalar (ordinary assignment: creates a container)
my $b := 1;
say $b.VAR.^name; # Int   (bind: no container created)
```

- `raku`: `Scalar` then `Int`.
- `mutsu` (`target/debug/mutsu`): `Scalar` then `Scalar`.

Verified directly with `raku -e` / `target/debug/mutsu -e`:

```
$ raku -e 'my $a = 1; say $a.VAR.^name; my $b := 1; say $b.VAR.^name;'
Scalar
Int
$ target/debug/mutsu -e 'my $a = 1; say $a.VAR.^name; my $b := 1; say $b.VAR.^name;'
Scalar
Scalar
```

## Root cause hypothesis

Per Raku semantics, ordinary assignment (`=`) to a `$`-sigiled `my` variable creates a
fresh `Scalar` container that holds the value; `:=` (bind) instead makes the variable
name directly refer to whatever is on the right-hand side, with no intervening
container — so `.VAR` on a bound variable returns the bound value itself (here, the bare
`Int` `1`), not a `Scalar` wrapper.

mutsu's compiler/VM appears to treat every `$`-sigiled `my` declaration uniformly as
getting a `Scalar` container, regardless of whether the initializer used `=` or `:=`.
The `:=` bind path needs to skip container creation and store the RHS value directly,
the same way it already does correctly for sigilless (`\x := ...`) bindings.

## Affected files (starting point)

- `src/compiler/` — wherever `:=` bind-declaration (`AssignExpr`/bind variant with a
  `$`-sigiled LHS) is compiled; compare against the sigilless `\x := ...` bind path,
  which already skips the container correctly.
