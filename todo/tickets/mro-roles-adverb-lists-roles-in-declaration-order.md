# `.^mro(:roles)` lists two composed roles in declaration order; raku reverses them

Found 2026-09-07 while fixing
`news/2026-09/mro-excludes-composed-roles.md`, which took the composed roles out
of the plain `.^mro`. It is the one row of that measurement matrix that still
diverges, and it is in the `:roles` variant rather than the plain one.

## Repro

```raku
role R2 { }
role R3 { }
class K2 does R2 does R3 { }
say K2.^mro(:roles).map({ .^name }).join(",");
# raku:  K2,R3,R2,Any,Mu
# mutsu: K2,R2,R3,Any,Mu
```

One composed role agrees (`K,R2,Any,Mu` in both), and a role composed into
another role agrees too (`KN,RB,RA,Any,Mu` for `role RB does RA; class KN does
RB`), so it is only the ordering of two roles composed by the SAME class.

raku lists them **last-declared first**, which is the same last-wins order
`.^roles` uses for a mixed-in role and that
`crate::value::types::mixin_roles_applied_last_first` already implements for
that case.

## Where to look

`Interpreter::classhow_mro_with_roles`
(`src/runtime/methods_classhow_mro.rs`) walks
`classhow_mro_names`'s output and splices each entry's roles in. The order it
splices them in comes from the class's `parents` / `class_does_only_roles`
registration order, which is declaration order.

Check whether the fix belongs there or one level down in registration: if
`parents` is also what decides method-resolution precedence between two roles
composed by one class, reversing it there would change dispatch, not just this
listing — measure `class K does R2 does R3` where both roles define the same
method before touching it (rakudo refuses that composition with an ambiguity
error, so the observable may be the error rather than a winner).

## Check when fixing

The repro; the single-role and role-in-role rows above stay as they are; the
plain `.^mro` stays role-free; `.^roles` order for the same class; and
`t/mro-excludes-composed-roles.t` still passes.
