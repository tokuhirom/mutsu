# A sub nested two levels deep reads its outer routine's parameter

```raku
sub d($p) { my sub t() { my sub u() { $p }; -> $p { u() } }; t()(0) }
say d(1);    # 1
```

A `my sub` declared inside a routine reads that routine's variables through
an alias each activation binds (#9111). For a readonly signature parameter,
the alias holds the parameter's value rather than a shared cell, because that
binding never changes (#17337). One level down the chain, the middle sub `t`
has such a value alias for `$p`. When `u` is declared inside `t`, its own
alias accepted only a shared cell, so it was left unbound and `u` resolved
`$p` by name in its caller's env. A closure or anonymous sub that had its own
`$p` therefore answered with that one, and mutsu printed `0`.

The nested sub's alias now takes the middle sub's value snapshot too. This is
#18248's fourth slice (shape D). It also covers the case where the call made
it to the right `$p` only by walking a closure frame's callers, which is the
dynamic-scope leak ADR-12529 retires.
