# `my ($g) = LIST;` gives `$g.VAR.^name` of `Int` instead of `Scalar`

Discovered via the doc-diff harness on `raku-doc/doc/Language/variables.rakudoc` (around line
134). The harness auto-bucketed this as `raku-drift-from-doc`, but re-verified directly with
`raku -e` — this is NOT drift, it's a real mismatch (the harness's drift heuristic misfired,
likely due to the doc's multi-statement OUTPUT block formatting).

## Repro

```
my ($g) = 7,8,9;
say $g;
say ( ($g) ).VAR.^name
```

- raku: `7` then `Scalar`
- mutsu: `7` then `Int`

`my ($g) = LIST;` is documented special syntax: parenthesizing a single variable in a `my`
declaration makes it a "list assignment to a List with one element" (as opposed to plain
`my $g = LIST;`, which would take just the first value in scalar context) — but the variable
`$g` itself should still be an ordinary `Scalar` container afterward, holding the value `7`. In
mutsu, `$g` ends up literally *being* the bare `Int` `7` (no `Scalar` container wrapper).

## Root cause guess

The parenthesized-single-variable declaration form (`my ($g) = ...`) likely takes a shortcut that
directly stores the extracted list-assignment value without wrapping it in a proper `Scalar`
container the way the ordinary `my $g = ...` declaration path does.

## Affected files (starting point)

- `src/compiler/` — `my (...)` destructuring-declaration compilation, specifically the
  single-variable-in-parens special case
- Compare to how `my $g = 7,8,9;` (no parens, which raku documents as scalar-context assignment
  taking a different value) wraps its target in a `Scalar` container correctly
