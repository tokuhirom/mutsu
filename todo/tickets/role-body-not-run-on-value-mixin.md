# A role's body is not run when the role is mixed into a value

`compose_role_on_value` (`src/runtime/types/roles.rs`) builds a `Mixin` value by
copying the `RoleDef`'s attributes, methods and marker keys onto the target. It
never touches `RoleDef::deferred_body_stmts`, so a role's non-declaration body
statements do not run on the mixin path at all:

```raku
class Ordinary { has $.x }
role Guarded[::T] {
    die "Need a CStruct" unless T.REPR eq 'CStruct';
    method describe() { "g" }
}
my $o = 5 but Guarded[Ordinary];
# raku:  dies with X::Role::Instantiation ("Could not instantiate role 'Guarded'")
# mutsu: succeeds
```

The composing and punning paths were fixed
([news](../../news/2026-07/role-body-guard-rejects-a-parameterisation.md)); this
is the remaining third path.

The narrow fix looks small — instantiate the concrete parameterisation before
mixing in, by calling `ensure_parametric_role_pun_class(role_name, role_args)?`
when the role actually has type parameters — and composition is memoised by the
pun class name, so repeated mixins would run the body once. But it is not a
one-liner in effect: it makes every `but R[T]` / `does R[T]` register a pun class
and evaluate the role body's lexical side effects in the mixing frame, which is a
behaviour change for every parameterised role with a body statement. That wants
its own measurement pass over roast, not a rider on an unrelated PR.

A second, related divergence found while fixing the pun path: a *non-parametric*
role's body statements run **twice** in mutsu — once at declaration
(`src/vm/vm_typedecl_ops.rs`, the `if type_params.is_empty()` block, which exists
so `role R { method foo {}; R.foo }` works) and again at composition
(`src/runtime/registration_class_decl.rs`). Rakudo runs them only at composition:

```raku
role R { say "BODY" }; class C does R { }
# raku:  BODY
# mutsu: BODY
#        BODY
```

Deciding where a non-parametric role body belongs is the same question as the
mixin path, so both should be settled together.

Repro for the double run: `mutsu -e 'role R { say "BODY" }; class C does R { }'`.
