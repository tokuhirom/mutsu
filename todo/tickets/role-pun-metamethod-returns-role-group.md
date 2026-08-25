# `R.^pun` returns the role *group*, not the punned class type object

## Repro

```raku
role R { method m { 42 } }
say R.^pun.HOW.^name;          # raku: Perl6::Metamodel::ClassHOW   mutsu: ...ParametricRoleGroupHOW
say R.^pun === R.new.WHAT;     # raku: True                          mutsu: False
```

Measured 2026-08-25 against `raku` and `target/debug/mutsu`.

## Root cause

`dispatch_classhow_dispatch`'s `"pun"` arm (`src/runtime/methods_classhow_dispatch.rs`)
calls `ensure_role_punned_to_class(&role_name)` — which registers the pun's `ClassDef`
under the role's own name — and then returns `Value::package(Symbol::intern(&role_name))`.
That value is the *role group* type object: mutsu's punned class and its role share one
registry name, so `Package("R")` is indistinguishable from the group, and `.HOW` correctly
answers the group's `ParametricRoleGroupHOW` for it.

The punned class type object does already exist in mutsu — it is the composition-keyed
anonymous type object [ADR-0060](../../docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md)
builds for `R.new.WHAT` (a `Mixin` over `Package("R")` sharing the process-wide
`mixin_what_cache` node for that composition). `R.new.WHAT.HOW` already reports `ClassHOW`
and `R.new.WHAT === R.new.WHAT` is already `True`. `^pun` simply does not return it.

## What the fix looks like

Make `^pun` return the same composition-keyed value `dispatch_what` produces for a punned
instance, so `R.^pun === R.new.WHAT`: after `ensure_role_punned_to_class`, build the pun's
composition markers (`__mutsu_role__{name}` plus `__mutsu_role_id__{name}` from
`registry().roles[name].role_id`) and resolve them through `mixin_what_value` /
`mixin_composition_overrides` (`src/runtime/methods_mixin_what_cache.rs`) against
`Package(name)` as the base.

## Why it is not folded into the three-ticket `.HOW` taxonomy fix

The three tickets fixed on 2026-08-25
(`news/2026-08/role-instance-how-wrong-metaclass.md` and its two siblings) were about
which metaclass an *existing* value reports. This one changes what `^pun` *returns* — from
a `Package` to a `Mixin` — which is a representation change for every consumer of `.^pun`,
and none of the three tickets' repros go through it. Worth auditing `.^pun` callers (both
in `src/` and in roast/`t/`) before changing the return type.

## Related

- `news/2026-08/role-instance-how-wrong-metaclass.md` (the sibling `.HOW`/`^mro` fix, which
  records this as its known remaining gap)
- `docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md`
- `src/runtime/types/role_candidate.rs` (individual role vs. role group identity)
