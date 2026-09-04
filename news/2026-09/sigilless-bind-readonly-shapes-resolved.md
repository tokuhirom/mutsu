# Both wrongly-read-only sigilless bind shapes now write through

`todo/deep/chained-and-array-element-sigilless-bind-wrongly-readonly.md` recorded
two `\x := ...` shapes that rejected a later write with `X::Assignment::RO`
where rakudo writes through to the ultimate source. Both now agree with rakudo,
and neither was closed by work aimed at this file:

```
my $a = 5; my \y := $a; my \x := y; x = 42; say "a=$a"   # rakudo a=42, mutsu a=42
my Int @arr = 1, 2, 3; my \x := @arr[0]; x = 1000; say @arr   # rakudo [1000 2 3], mutsu [1000 2 3]
```

- **Shape 2** (the typed array element) was already passing at the 2026-09-04
  TRIAGE regeneration, which recorded it and noted that the file's
  `mark_readonly` hypothesis had gone stale with it.
- **Shape 1** (the two-hop chain) was closed by
  `news/2026-09/sigilless-bind-chain-takes-on-its-sources-binding.md`. It was two
  defects, not the read-only status this file blamed: the parser's
  "can this RHS denote a container?" filter never admitted a bareword source at
  all, and the store then resolved such a source through the
  `__mutsu_sigilless_alias::` chain, which only records a link to a NAMED
  variable — so an element alias dropped the write into a copy and a value
  binding looked writable.

Pinned by `t/sigilless-bind-chain.t` (14 assertions) and
`t/bind-alias-is-a-container.t`.
