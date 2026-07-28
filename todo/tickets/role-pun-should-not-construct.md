# A role pun should never construct an instance

Calling a method on a role type object puns the role into a class and runs the
method on that class's **type object**. mutsu puns by *constructing*: it calls
`dispatch_new` on the role and dispatches to the resulting instance
(`methods_call_dispatch.rs`, the "Role type-object method punning" block).

The observable divergence:

```raku
role C { has $.x = 5; method get { $!x } }
say C.get;
# raku:  Cannot look up attributes in a C type object. Did you forget a '.new'?
# mutsu: 5
```

The half of this that broke real code — a role that declares its own `new` lost
the arguments of every *other* method called on the pun, because punning called
that `new` with none — is fixed
([news](../../news/2026-07/role-pun-does-not-run-the-roles-own-new.md)). Only
roles that declare `new` take the type-object path; everything else still puns
by constructing.

## What blocks finishing it

Switching the no-`new` case over as well fails
`roast/S13-overloading/typecasting-long.t` (the `rakudo#4094` block at the end),
in two successive ways:

1. **Duplicated candidates.** `my role R04 does R01 { }` — composition already
   copies R01's methods into R04's own table, and
   `ensure_role_punned_to_class` (`registration_class_augment.rs`) then extends
   them again from `role_parents`. `R04.()` reports four matching `CALL-ME`
   signatures for R01's two multis. Making that extend an
   `or_insert_with` fixes the duplication.

2. **Type-object matching ignores composed roles.** With the duplication gone,
   `R04.()` becomes "No such method 'CALL-ME' for invocant of type 'R04'":
   R01's candidates are `multi method CALL-ME(::?ROLE:U:)`, i.e. their invocant
   is constrained to `R01:U`, and an `R04` *type object* does not match it. An
   `R04` *instance* does, which is why the constructing pun worked. So the
   remaining work is in the acceptance check for a `Package` value against a
   role constraint — `class_composed_roles` is consulted for the distance
   ranking (`dispatch_candidates.rs`) but not, apparently, for the match itself.

Both are worth fixing on their own; together they should let the pun always
dispatch on the type object, at which point the `role_declares_new` special case
in `methods_call_dispatch.rs` can go away and reading an instance attribute
through a pun can start erroring like raku.

## Repro

```raku
my role R01 {
    multi method CALL-ME(::?ROLE:U:) { 'no-arg' }
    multi method CALL-ME(::?ROLE:U: \v) { 'arg:' ~ v }
}
my role R04 does R01 { }
say R04.();     # both spellings must work
say R04.(3);
```
