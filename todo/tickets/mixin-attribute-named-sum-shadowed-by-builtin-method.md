# A mixin attribute accessor named `sum` (and likely other Cool/List method names) is shadowed by the builtin

```raku
role R[$a, $b] { has $.sum; submethod TWEAK { $!sum = $a + $b } }
my $q = 1;
$q does R[10, 20];
say $q.sum;   # raku: 30   mutsu: 1  (the ORIGINAL $q's value, via the builtin List/Cool .sum)
```

Renaming the attribute (`$.total` instead of `$.sum`) makes it work correctly
— the TWEAK/BUILD write-back itself is fine (confirmed with
`MUTSU_TRACE`-style prints: `$a`/`$b` are seeded correctly and `$!sum = $a +
$b` runs). The read afterward, `$q.sum`, resolves to the builtin
`sum`-family method (`List`/`Cool.sum`, which reduces a scalar to itself) on
the underlying `Int`, not the role's own attribute accessor.

Found while fixing
`todo/tickets/role-submethod-runtime-does-parameterized-value.md` (2026-08-15)
— unrelated to that bug, a pure accessor-dispatch shadowing issue.

## Scope

Only reproduces via the **runtime `does`/`but` on a non-Instance value**
(mixin composition). Class-based composition does not have this problem:

```raku
role R2 { has $.sum; }
class D does R2 { submethod TWEAK { $!sum = 42 } }
say D.new.sum;   # 42 in both raku and mutsu
```

## Where to look

The mixin method-dispatch fallback (wherever a builtin `Cool`/`List` method is
tried before — or instead of — a mixin's own `__mutsu_attr__{name}`-backed
accessor). Likely the same dispatch chokepoint
`run_role_submethod`/`call_role_build_submethods` write back through
(`src/runtime/types/roles.rs`), on the READ side rather than the write side.
