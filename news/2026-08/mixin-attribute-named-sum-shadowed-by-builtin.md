# A runtime mixin's public attribute accessor no longer loses to a same-named Cool/List builtin method

```raku
role R[$a, $b] { has $.sum; submethod TWEAK { $!sum = $a + $b } }
my $q = 1;
$q does R[10, 20];
say $q.sum;   # raku: 30   mutsu (before this fix): 1
```

`$q.sum` resolved to the builtin `List`/`Cool.sum` method on the underlying
`Int` instead of the role's own `$.sum` attribute accessor. Renaming the
attribute (`$.total`) made it work — the `TWEAK`/write-back mechanics were
fine; this was purely a read-side dispatch-ordering bug, and only reproduced
via a **runtime** `does`/`but` mixin (class-based `does` composition already
worked correctly).

## Root cause

`should_bypass_native_fastpath` (`src/runtime/methods_native_bypass.rs`)
decides whether to let a Mixin value fall through to the builtin
`native_method_0arg`/`native_method_1arg`/`native_method_2arg` fast path
before ever reaching the mixin's own `__mutsu_attr__`-backed accessor lookup
further down `call_method_with_values`. Its `mixin_role_has_method` helper —
whose whole purpose is to say "no, don't bypass, this role provides its own
answer for this method name" — only checked `role.methods.contains_key(method)`.
A `has $.sum` attribute declaration doesn't add an entry to `role.methods` at
all (its accessor is auto-generated, not a declared method), so the check
missed it, `should_bypass_native_fastpath` returned `false`, and the native
`Cool.sum` fast path won the race.

## Fix

`mixin_role_has_method` now also checks the role's public attributes
(`role.attributes.iter().any(|a| a.is_public && a.name == method)`), mirroring
the same `is_public && a.name == method` pattern already used elsewhere in
this codebase for role-attribute-as-accessor checks (e.g. the `ParametricRole`
dispatch arm in `call_method_with_values`).

Regression tests: `t/mixin-attribute-shadowed-by-builtin-method.t`.
