# A bound alias is writable from a sub whatever its caller's parameters are called

```raku
my $src = 10;
my $alias := $src;
sub bump-alias { $alias++ }
sub shadow-alias($alias) { bump-alias() }
shadow-alias(0);
say $src;   # 11
```

This used to die with "Cannot resolve caller postfix:<++>(alias); the parameter
requires mutable arguments" (#11539). `bump-alias` writes its free variable
`$alias` by name. The cell the `:=` bind had shared between `$src` and
`$alias` recorded no decision about its writability. So the write asked the
name-keyed readonly registry, and there the caller's readonly parameter
`$alias` answered.

A `my $alias := $src` declaration now decides that shared cell writable when
the source is writable, as a plain `my` declaration does (ADR-11142 §2.3).
The cell's own decision answers before the registry is consulted. An alias of
a readonly parameter is left undecided, so writes through it are still
refused.
