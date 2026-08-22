# `$metadata.can($metadata, "uc")` returns `()` instead of `(uc uc)` when `$metadata` is a `.HOW`

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Language/structures.rakudoc:458` — the "Introspection" section's own worked example).

## Repro

```raku
my $any-object = "random object";
my $metadata = $any-object.HOW;
say $metadata.^mro;                   # OUTPUT: «((ClassHOW) (Any) (Mu))␤» -- matches raku
say $metadata.can( $metadata, "uc" ); # OUTPUT: «(uc uc)␤»
```

- `raku`: `(uc uc)`
- `mutsu` (`target/debug/mutsu`): `()`

## Minimal isolation

```raku
my $s = "x";
my $m = $s.HOW;
say $m.can($m, "uc");   # raku: (uc uc); mutsu: ()
```

For contrast, passing the *original* instance (not the HOW itself) as the
`can`-argument already works correctly in mutsu:

```raku
my $s = "x";
say $s.HOW.can($s, "uc"); # raku AND mutsu: (uc)
```

So the bug is specific to passing the metaclass object itself (`$metadata`/`$m`) as
the first argument to its own `.can` — Rakudo's `ClassHOW.can(invocant, name)`
apparently resolves against the type `invocant` *represents* rather than literally
introspecting `invocant`'s own type when `invocant` happens to be a HOW object, and
returns two matches (`uc uc`) where the direct-instance call above returns only one
(`uc`) — suggesting Rakudo's dispatch here is doing something more elaborate (walking
both the represented type's MRO and possibly a self-referential HOW hierarchy) that
mutsu's `can` implementation doesn't replicate.

## Affected files (starting point)

- Wherever `Metamodel::ClassHOW.can` / `.^can` is implemented — grep for `"can"` in
  `src/runtime/class_introspection.rs` or `src/runtime/methods_classhow_lookup.rs`.
