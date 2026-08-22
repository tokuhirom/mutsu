# `Int but Str` value's `.^name` reports the `IntStr` allomorph type instead of raku's `Int+{<anon|1>}`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:1991`).

## Root cause hypothesis

`42 but 'forty two'` mixes a role-like string identity onto an `Int` value. Rakudo represents
this as an anonymous mixin type built on the fly (`Int+{<anon|1>}` — `Int` composed with an
anonymous role holding the `Str`-role behavior), which is what `.^name` reports.

mutsu instead appears to special-case `Int but Str-literal` and route it to the pre-existing
built-in `IntStr` allomorph representation (the same type used for e.g. `<42>` numeric
literals that carry both numeric and string forms). That is a plausible representation for the
*value* (stringifies/numifies correctly — `+33` and `.Str` both work fine in the repro), but it
leaks through `.^name`, which should report the anonymous-mixin name, not the allomorph type
name.

## Minimal repro

```raku
my $forty-two = 42 but 'forty two';
say $forty-two+33;    # 75 in both
say $forty-two.^name; # raku: Int+{<anon|1>}   mutsu: IntStr
say $forty-two.Str;   # forty two in both
```

- `raku`: `Int+{<anon|1>}`
- `mutsu` (`target/debug/mutsu`): `IntStr`

Only the `.^name` line diverges; the arithmetic and stringification results already match.

## Affected files (starting point)

Likely wherever `but` mixin construction chooses a representation for an `Int`/numeric LHS
mixed with a `Str`/literal RHS — probably in the mixin (`but`) handling in
`builtins/arith.rs`/`runtime/` mixin dispatch, where it likely takes a shortcut to the
`IntStr` allomorph instead of building a proper anonymous-mixin type whose `.^name` reflects
`Int+{<anon|N>}`.
