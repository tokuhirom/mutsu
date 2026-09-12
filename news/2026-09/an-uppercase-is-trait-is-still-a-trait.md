# An uppercase `is Trait` is still a trait, and a natively-modelled core type is still a parent

The ecosystem parity ledger grouped 20 distributions under one cluster,
[#7996](https://github.com/tokuhirom/mutsu/issues/7996) — "a class cannot
inherit from a type declared in the same or an imported unit". The census the
ticket asked for turned out to be the whole job: those 20 failures are **five
different root causes**, not one, and they do not need the same fix. Three of
them are closed here; one had already been closed on `main` after the sweep
measured; the fifth is the real content of the cluster and is re-filed narrowly
with its own minimised repros.

## 1. `class Foo is Static { }` never reached `trait_mod:<is>`

Raku spells a class-level trait `is Foo`, and decides what that means from
whether `Foo` names a known type. When it does not, `is Foo` is not inheritance
at all — it desugars to the **named** argument `trait_mod:<is>($type, :Foo)`.
That is the entire public interface of the `Staticish` distribution:

```raku
use Staticish;
class Foo is Static { has Str $.bar is rw; }   # multi trait_mod:<is>(Mu:U, :$Static!)
```

`validate_class_parents` did defer an unknown parent to that dispatch, but only
when the name began with an **ASCII lowercase letter**. Trait names written the
way Raku programs actually write them — capitalised — fell straight through to
`X::Inheritance::UnknownParent`. The restriction is gone: any unknown `is`
parent is deferred whenever the program defines a `trait_mod:<is>` at all. That
is safe because the dispatch site already converts a no-matching-candidate
verdict back into `unknown_parent_error` (`vm_typedecl_ops.rs`), so a genuine
typo still raises `X::Inheritance::UnknownParent` — `t/oo/trait/
user-trait-mod-does-not-consume-every-trait.t` pins exactly that and still
passes unchanged.

One ordering change came with it. The `package A {}; class B is A {}` →
`X::Inheritance::Unsupported` check now runs **before** the deferral rather than
after. A declared package is not an unknown name, so rakudo hands it to
`trait_mod:<is>` positionally, as the package object — never as the `:Name`
named argument the deferral synthesises.

## 2. The deferred name was still becoming a parent

Widening the deferral exposed a second bug that had been sitting on the
lowercase path all along: the trait name stayed in the class's inheritance
parent list. `class Alpha is Marked { }` gave mutsu

```
Alpha.^parents   (Marked,)          # rakudo: ()
Alpha.^mro       Alpha,Marked,Any,Mu # rakudo: Alpha,Any,Mu
```

— a phantom ancestor for every class that uses a class-level trait. A deferred
name is a trait, never a parent, so it now joins the `does`-role-sharing-the-
class's-own-name case in the set `validate_class_parents` returns for
`begin_class_def` to filter out of the C3 parents. That set was called
`self_named_does_roles`; it now holds both kinds and is called
`non_inheritance_parents`.

And a deferral that turns out to have been wrong must leave no trace at all.
The class shell is published *before* the dispatch runs — the trait handler has
to be able to see the type object — so by the time the no-candidate verdict
comes back, `register_class_decl`'s own rollback snapshot has gone out of
scope. It now hands that snapshot on through
`Interpreter::deferred_trait_class_rollback`, and `exec_register_class_op`
restores it before raising `unknown_parent_error`. Without that, a failed
`class B is NoSuchParent { }` left `B` registered and the next genuine `class
B` died as a redeclaration — and because merely importing `Test` puts a
`trait_mod:<is>` in scope, that reached ordinary test files: it is exactly what
`t/oo/class/inheritance-unsupported.t` hit on this change's first full run.
`ClassRegSnapshot::restore` is shared with the body-failure path and
deliberately rewinds only the registry columns that path owns, so the rollback
also drops the two "this name is a user-declared type" markers a
from-nothing declaration wrote.

The snapshot is taken into a local immediately after registration rather than
read off the field at the dispatch site: a nested declaration in the class's
own body (`class Outer is Outerish { class Inner { } }`) runs its own
`RegisterClass` op first and would otherwise have overwritten it with
`Inner`'s.

## 3. A core type mutsu models natively could not be a parent

`class ValueClass::Attribute is Attribute { }` died as an unknown parent, and
with it the `ValueClass` distribution and `Functional::Queue` /
`Functional::Stack`, which depend on it. `Attribute` is a perfectly real type in
mutsu — `::('Attribute')` yields the type object and
`Interpreter::is_builtin_type` knows it — it simply has no `ClassDef` in
`registry.classes`, and the existence check vouched for a parent only through
`registry.classes`, `BUILTIN_PARENT_TYPES`, the core-role oracle,
`registry.roles` and `registry.enum_types`.

`BUILTIN_INHERITABLE_TYPES` fills that hole with the 26 names in the same
position, each checked against rakudo 2026.07 with `class Zz is <Name> { }`
(rakudo compiles all of them): `Attribute`, `CallFrame`, `CompUnit`,
`CX::Return`, `CX::Warn`, `Cursor`, `Deprecation`, `Duration`, `Instant`,
`Label`, `NFC`/`NFD`/`NFKC`/`NFKD`, `ObjAt`, `Scalar`, `StrDistance`,
`Submethod`, `Uni`, and the seven metamodel HOWs that were not already listed.
Two more distributions come off the same hook: `Timezones::ZoneInfo`
(`class CX::Warn::Timezones::UnknownID is CX::Warn { }`) and `Protocol`
(`class MetamodelX::Protocol is Metamodel::SubsetHOW { }`).

The list is deliberately **separate** from `BUILTIN_PARENT_TYPES` rather than
folded into it. That table also decides whether a `does` target is composable,
and whether a `but`-mixin may take the class-declaration path instead of the
wrapper one — `types::role_mixin_class`'s own comment names `Attribute` as the
example that must keep the wrapper. Only the `is`-parent existence check
consults the new list, so `class Nope does Attribute { }` is rejected exactly as
before and `Holder.^attributes[0] but Tag` still takes the wrapper path.

## 4. Already fixed, ledger stale

`Test::Async::Metamodel::BundleHOW is Metamodel::ParametricRoleHOW` and
`OO::Plugin::Metamodel::PlugRoleHOW is Metamodel::ParametricRoleHOW`
(`Test::Async`, `OO::Plugin`, `Config::BINDish`) are not a gap:
`Metamodel::ParametricRoleHOW` was added to `BUILTIN_PARENT_TYPES` by `5926cbbc`
at 2026-09-11 23:58 UTC, minutes after the sweep that produced the cluster
measured `7807eb5`. Both declarations compile on `main` today.

## 5. What is left: `also is` is applied too early

The remaining members are one root cause with a different shape, and it is
**not** the inheritance check — it is *when* mutsu applies `also is Parent`.
Rakudo executes that statement at its position in the class body; mutsu's parser
hoists it into the class header (`stmt_also_is_parent` /
`push_also_is_parent`), so the parent is validated before any of the body has
run. Everything the body would have established first is therefore invisible:

```raku
class Kid {
    use Parentish;          # the parent arrives HERE
    also is Parentish;
}                           # mutsu: 'Kid' cannot inherit from 'Parentish' because it is unknown.
```

```raku
unit class UC;
class Inner is Positional { }   # a nested class of UC
also is Inner;                  # mutsu: 'UC' cannot inherit from 'Inner' because it is unknown.
```

The first shape is every one of `Font::AFM`'s 14 `Font::Metrics::*` compunits;
the second is `Intl::CLDR`'s five `CLDR::*` types. Filed as
[#8099](https://github.com/tokuhirom/mutsu/issues/8099) with both repros, since
the fix is a different change in a different layer (mark the `also is` parents
through the declaration plan, defer their validation until after the body walk,
and add them before `finalize_class_registration` computes the MRO) from
anything in this one.

The rest of the cluster's 20 are cascades: the named parent's own compunit fails
to load for an unrelated reason (`PDF::Content::Font::Enc`'s missing
`NativeCall::Types`, `CSS::Grammar::AST`, `Parse::Paths`'s `BasePaths`), so the
parent genuinely does not exist and the inheritance error is the symptom, not
the disease.

## Pins

- `t/oo/trait/uppercase-is-trait-reaches-trait-mod.t` — 12 assertions: the
  uppercase trait fires its handler, the name reaches neither `^parents` nor
  `^mro`, the lowercase spelling still works and is likewise absent from the
  MRO, a name no candidate claims is still `X::Inheritance::UnknownParent`, an
  error from inside a matching handler still propagates, a known uppercase
  parent is still ordinary inheritance, a failed trait declaration frees its
  name again, and a deferred-trait class may still declare nested types.
  Passes verbatim under rakudo. The
  role-side mirror (`role R is Marked { }`) is still broken and is
  [#8100](https://github.com/tokuhirom/mutsu/issues/8100): `RoleParentOp` does
  not record `is` vs `does`, so widening the deferral there would change the
  error for a `does` typo too.
- `t/oo/class/builtin-inheritable-parent-types.t` — all 26 new names as an `is`
  parent, plus the three things that must NOT move: an unknown parent is still
  `X::Inheritance::UnknownParent`, `does Attribute` is still rejected, and a
  `but`-mixin on an `Attribute` still keeps the wrapper path. Also passes
  verbatim under rakudo.
