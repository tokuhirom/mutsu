# A role-composed attribute's `is default(...)` now restores after Nil

A directly-declared class attribute's `is default(...)` restores correctly
when `Nil` is assigned to it, but a role-composed one did not:

```raku
role R { has $.w is default(21) is rw; }
class Consumer does R { }
my $obj = Consumer.new;
say $obj.w;        # 21 — correct
$obj.w = Nil;
say $obj.w;         # was printing Nil; now correctly prints 21
```

The root cause was a second registry table. A directly-declared attribute's
`is default(...)` is evaluated once at registration and cached as a `Value`
in `Registry::class_attribute_defaults`. A role attribute's default may
reference the role's type parameter (`is default(T)`), so it can't be
evaluated until the role is composed into a class — it is instead copied
onto the consuming class as a raw `Expr` in a separate table,
`Registry::class_attribute_default_exprs`, evaluated on demand later.

Every Nil-restore call site (the per-method-call `$!attr`/`$.attr` default
seeding in `vm_method_dispatch.rs`, and the direct rw-accessor-assignment
paths in `methods_mut_method_lvalue.rs`/`builtins_multidim_assign.rs`) only
ever consulted the `Value` table, so a role-composed attribute's default was
silently treated as absent. A new helper,
`Interpreter::class_attribute_default_with_role_fallback`, falls back to
evaluating the `Expr` table when the `Value` table misses (mirroring the
fallback `apply_container_attribute_defaults` already used for `@`/`%`
element defaults), and all seven call sites now go through it. The
`any_attr_defaults` fast-gate in `vm_method_dispatch.rs` was widened to also
check the `Expr` table, since a program with only role-attribute defaults
(no directly-declared ones) previously skipped the whole mechanism.

Found while writing a regression test for ADR-0019 D2c-1
(`news/2026-08/d2c1-is-default-attribute-chunk.md`); unrelated to that
change and fixed separately.
