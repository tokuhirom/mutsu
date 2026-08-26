# `R.^pun` returns the role *group*, not the punned class type object

## Repro

```raku
role R { method m { 42 } }
say R.^pun.HOW.^name;          # raku: Perl6::Metamodel::ClassHOW   mutsu: ...ParametricRoleGroupHOW
say R.^pun === R.new.WHAT;     # raku: True                          mutsu: False
```

Re-measured 2026-08-26 against `raku` v2026.06 and `target/debug/mutsu`; still
reproduces. The full measured divergence table for `role R { method m { 42 } }` is:

| expression | raku | mutsu |
|---|---|---|
| `R.^pun.^name` | `R` | `R` |
| `R.^pun.HOW.^name` | `Perl6::Metamodel::ClassHOW` | `Perl6::Metamodel::ParametricRoleGroupHOW` |
| `R.^pun === R.^pun` | `True` | `True` |
| `R.^pun === R.new.WHAT` | `True` | **`False`** |
| `R.^pun.^mro` | `((R) (Any) (Mu))` | `((R) (Any) (Mu))` |
| `R.^pun.^roles` | `((R))` | **`()`** |
| `R.^pun.new.m` | `42` | `42` |
| `R ~~ R.^pun` | `False` | **`True`** |
| `R.^pun ~~ R` | `True` | `True` |
| `R.^pun.^candidates` | X::Method::NotFound | **`(R.new)`** |
| `C === R.^pun` for `class C does R {}` | `False` | `False` |

Every mutsu divergence above is a consequence of the same thing: `^pun` hands back the
role GROUP's type object, so the group's metaclass, its `.^candidates`, and its
"everything smartmatches me" behaviour all leak into what should be an ordinary class.

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

## Caller audit (done 2026-08-26) — this is why it is still deferred

`.^pun` is produced in exactly one place (the arm above) and consumed in five, **three of
them in whitelisted roast files**. Changing the representation from `Package` to `Mixin`
is not a local change; each of these has to keep working:

1. `roast/6.c/S12-class/mro-6c.t:78,88` (whitelisted) —
   `is-deeply C4.^mro_unhidden[0..*-3], (C4, C3, C2, C1, R3b.^pun)`. The MRO walk
   (`classhow_mro_names` / `classhow_mro_with_roles`,
   `src/runtime/methods_classhow_dispatch.rs`) emits `Package(role_name)` for a punned-role
   level. If `^pun` becomes a `Mixin`, `is-deeply` fails unless the MRO emits the *same*
   composition-keyed value. So the pun representation and the MRO representation must
   change together.
2. `t/mro-role-hides.t:14,46` — the local twin of the same assertion.
3. `roast/S12-coercion/coercion-methods.t:102,106` (whitelisted) —
   `isa-ok $obj, R1.^pun`. `isa-ok`'s expected-type argument would become a `Mixin`, so
   the `isa` check has to accept a composition-keyed type object as the RHS.
4. `roast/S02-types/flattening.t:147` (whitelisted) —
   `my @types = array, Array, Iterable.^pun, List, Range, Supply;` then
   `is-deeply $type.flat, ($type,)` and `$type.^name`. Two extra requirements: `^pun` must
   work for a **built-in role with no `RoleDef`** (`ensure_role_punned_to_class` returns
   early for those, so today it just hands back `Package("Iterable")`), and the resulting
   value must survive `.flat` / `.^name` as an ordinary type object.
5. No `src/` consumer other than the producing arm.

The sibling `.HOW`/`^mro` taxonomy fix (`news/2026-08/role-instance-how-wrong-metaclass.md`,
PR #6976) deliberately left this out for the same reason, and the six-ticket role-composition
batch of 2026-08-26 did too: the other six were independent, this one is a cross-cutting
representation change that should be its own PR with the MRO emission changed in the same
commit.

## Related

- `news/2026-08/role-instance-how-wrong-metaclass.md` (the sibling `.HOW`/`^mro` fix, which
  records this as its known remaining gap)
- `docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md`
- `src/runtime/types/role_candidate.rs` (individual role vs. role group identity)
