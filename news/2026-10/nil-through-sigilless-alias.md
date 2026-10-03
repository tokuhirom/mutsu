# Nil stored through a sigilless alias decays against the bound container

A sigilless name — a `sub t(\p)` parameter or a `my \g := $f` binding — is the
container it was bound to, so assigning `Nil` through it now stores what a
direct assignment to that container would: its `is default`, its type object,
or `Any`.

```raku
my $q = 1; sub t(\p) { p = Nil }; t($q); say $q.raku;           # Any (was Nil)
my $d is default(15) = 1; sub u(\p) { p = Nil }; u($d); say $d;  # 15  (was Nil)
```

mutsu reaches a sigilless alias's target through the by-name
`__mutsu_sigilless_alias::` chain rather than a shared cell, and the parameter
store skipped the Nil reset entirely, so the raw `Nil` was mirrored into the
caller's variable. The `SetLocal`, `AssignExprLocal` and `SetGlobal` stores now
follow the chain to its root and decay the `Nil` against the root's metadata
(#11110), the by-name counterpart of the cell-carried default from #9831.
