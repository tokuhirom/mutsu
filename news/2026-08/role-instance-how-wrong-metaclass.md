# A punned role's instance reports `ClassHOW`, and its MRO head is the punned class

`role R { method m { } }; R.new.^mro[0].HOW.^name` reported
`Perl6::Metamodel::ParametricRoleGroupHOW` where Rakudo reports
`Perl6::Metamodel::ClassHOW`.

## What was actually wrong (the ticket's guess was close but not it)

The ticket guessed mutsu's `.new`-on-a-role path "returns/reuses the role's own
`ParametricRoleGroupHOW`-tagged type object directly instead of synthesizing an anonymous
class wrapper". Measurement showed mutsu's punning is in fact nearly complete already:
`R.new.^name`, `.^mro`, `.^roles`, `R.new ~~ R`, `R.new.WHAT !=== R`, and pun caching
(`R.new.WHAT === R.new.WHAT`) all matched Rakudo before this change. Two narrower defects
produced the reported symptom.

**1. `.HOW` answered a role metaclass for a non-type-object.** `dispatch_how()` derived a
type *name* from the value (a `Package`'s symbol, an `Instance`'s class name, a `Mixin`'s
inner instance's class name) and then asked `registry().roles` about that name. Because
`ensure_role_punned_to_class` registers the pun under the role's own name, the name `R` is
simultaneously a role and a class — so an *instance* of the pun answered with the role
group's HOW. An instance is never a role type object; the role group lives on the name,
not on the values made from it. The role branch is now gated on the target actually being
a type object (`ValueView::Package`), and role candidates are recognised explicitly
(see `role-declaration-expression-yields-group-not-parametric-role.md`).

**2. `^mro`'s head was named, not taken.** The `mro` handler built every entry as
`Value::package(name)` from `classhow_mro_names`. Naming the head by its class name is
only equivalent while a name has exactly one type object behind it — and it does not for a
punned role (the name `R` is the role *group*, while the instance's type is the punned
class) nor for a role-mixed value (`(1 but R)`, whose type is `Int+{R}`, not `Int`). The
head is now taken from the invocant's own `.WHAT`, which already answered all three
correctly. `C.^mro[0] === C` for a class and `$o.^mro[0] === $o.WHAT` for an instance are
exactly Rakudo's rule, so this also fixed `(1 but R).^mro[0].^name`, which reported `Int`
instead of `Int+{R}`.

## Known remaining gap (closed)

`R.^pun` used to still return the role group's `Package("R")` rather than the punned
class type object, so `R.^pun.HOW` reported the group HOW and `R.^pun === R.new.WHAT`
was `False` (Rakudo: `ClassHOW` and `True`). That was a different entry point with its
own representation problem, filed separately and since fixed — see
`news/2026-08/role-pun-metamethod-returns-punned-class.md`.

Fixed alongside `role-declaration-expression-yields-group-not-parametric-role.md` and
`metamodel-parametricrolehow-new-type-wrong-how.md`; pinned by
`t/metamodel-role-how-taxonomy.t`.
