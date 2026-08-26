# `R.^pun` returns the punned CLASS, not the role group

`R.^pun` previously handed back the role GROUP's own type object rather than
the punned class it is supposed to name. `ensure_role_punned_to_class`
registers a role's punned `ClassDef` under the role's own name, so
`Value::package(Symbol::intern(&role_name))` was ambiguous between "the role
`R`" and "the class `R` was punned into" — and mutsu's `.HOW` dispatch always
resolved that ambiguity toward the role, since `registry().roles` still
contains the name after punning. That leaked the role group's metaclass and
behaviour into what should be an ordinary class:

```raku
role R { method m { 42 } }
say R.^pun.HOW.^name;          # raku: Perl6::Metamodel::ClassHOW   mutsu (before): ...ParametricRoleGroupHOW
say R.^pun === R.new.WHAT;     # raku: True                         mutsu (before): False
say R.^pun.^roles;             # raku: ((R))                        mutsu (before): ()
say R ~~ R.^pun;                # raku: False                        mutsu (before): True
say R.^pun.^candidates;         # raku: throws X::Method::NotFound   mutsu (before): (R.new)
```

## The fix

The punned class type object already existed elsewhere in mutsu: it is the
composition-keyed anonymous type object
([ADR-0060](../../docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md))
`R.new.WHAT` produces — a `Mixin` over `Package("R")` carrying
`__mutsu_role__R` (and `__mutsu_role_id__R` when the role has a minted id),
resolved through the process-wide `mixin_what_cache`. `^pun` simply never
built that same value.

`Interpreter::punned_role_type_object` (`src/runtime/methods_mixin_what_cache.rs`)
is the new shared helper: it calls `ensure_role_punned_to_class`, builds the
same composition markers `mark_punned_role_instance` stamps on a real punned
instance, and resolves them through the same `mixin_composition_key` /
`mixin_composition_overrides` machinery ADR-0060 already uses for `.WHAT`.
`^pun`'s `"pun"` arm (`methods_classhow_dispatch.rs`) now just calls it, so
`R.^pun` and `R.new.WHAT` are literally the same cached `Mixin` value.

A pre-existing gap surfaced while wiring this up: `mark_punned_role_instance`
and the role-declares-its-own-`new` construction path never recorded
`__mutsu_role_id__{name}` on a plain (non-parametric) pun, even though the
parameterised-pun path always did. Left alone, `R.^pun`'s freshly-computed
composition key (which does include the role's minted id) would never match
`R.new.WHAT`'s key (which never included it), so both call sites were fixed
to record it consistently — every punned instance and `^pun` itself now key
to the same cache entry.

## Keeping all five consumers working

Changing `^pun`'s representation from `Package` to `Mixin` required updating
every consumer in the same commit, since it is a cross-cutting
representation change:

1. **MRO emission** (`roast/6.c/S12-class/mro-6c.t`, `t/mro-role-hides.t`) —
   `class C3 is R3a is R3b { }` puns a role used as an `is` parent, and its
   bare role-name level in the MRO chain used to be emitted as
   `Value::package(role_name)`, matching `^pun`'s old representation by
   coincidence. `Interpreter::mro_names_to_values`
   (`src/runtime/methods_classhow_mro.rs`) now checks whether an MRO-chain
   entry's name is ALSO a registered role and, if so, calls the same
   `punned_role_type_object` helper `^pun` uses — so `C4.^mro_unhidden[0..*-3]`
   still `is-deeply`s against `(C4, C3, C2, C1, R3b.^pun)`.
2. **`isa-ok`** (`roast/S12-coercion/coercion-methods.t`) — needed no code
   change: `test_fn_isa_ok`'s type-name extraction already had a fallback
   (`what_type_name`) for a non-`Package` RHS, and `what_type_name` on a
   `Mixin(Package("R1"), {__mutsu_role__R1})` already excludes a role marker
   matching the base type's own name (the existing "a punned role names
   itself, not `R+{R}`" rule), so it resolves to plain `"R1"` exactly as
   before.
3. **Built-in role with no `RoleDef`** (`roast/S02-types/flattening.t`,
   `Iterable.^pun`) — turned out NOT to need special-casing:
   mutsu registers a `RoleDef` (with `role_id: 0`) for `Iterable` at runtime
   init, so `ensure_role_punned_to_class` already puns it like any
   user-declared role; `punned_role_type_object` just omits the (absent)
   role-id marker. What DID need a fix was `.flat`: mutsu's generic
   `Mixin` method dispatch unconditionally delegated any method it didn't
   special-case straight to the mixin's `inner` value, discarding the
   wrapper — so `Iterable.^pun.flat` silently unwrapped back to the bare,
   ambiguous `Package("Iterable")`. This was a pre-existing bug for ANY
   `Mixin` value (`(1 but R).flat[0].^name` was `Int` instead of `Int+{R}`
   even before this change), just never exercised until `^pun` started
   returning one. Fixed by giving `flat_val` (`src/builtins/functions/flat.rs`)
   a `Mixin` arm: flatten through a container inner (Array/Seq/Slip/Range/
   Hash — so `(%h does R).flat` still spills `%h`'s pairs, matching raku)
   and otherwise preserve the whole mixin as the flat element (so
   `Iterable.^pun.flat` is `(Iterable.^pun,)`, and `(1 but R).flat[0].^name`
   is `Int+{R}`), plus routing `.flat` in `native_method_0arg`'s `Mixin`
   dispatch (`src/builtins/methods_0arg/mod.rs`) through that logic with the
   whole mixin rather than through the blanket `inner` delegation.
4. **`.^candidates`** — not in the original caller audit, but discovered
   while re-measuring the ticket's divergence table: `R.^pun.^candidates`
   must throw `X::Method::NotFound` (candidates is a role-group-only MOP
   method, undefined on `ClassHOW`), but the `"candidates"` arm in
   `dispatch_classhow_method` matched ANY receiver whose extracted name was
   a role — including a punned `Mixin`, via its `to_string_value` fallback.
   Gated behind a new `Interpreter::is_role_reference_value` check so only a
   genuine role reference (a role's own `Package`, a `ParametricRole`, or a
   `.^candidates`-returned candidate `Instance`) matches; a punned `Mixin` or
   an ordinary class now correctly falls through to the method-not-found
   default.
5. No other `src/` consumer exists.

## Verification

A new regression test, `t/role-pun-metamethod-identity.t`, exercises every
row of the ticket's divergence table (identity, `.HOW`, `.^mro`, `.^roles`,
constructing through the pun, smartmatch in both directions, `.^candidates`
throwing, a `does`-composing class not being identical to the pun, the
built-in-role case, and `.flat` on both a punned type object and an ordinary
`but`/`does`-mixed value) and passes identically under `raku` and `mutsu`.

`news/2026-08/role-instance-how-wrong-metaclass.md`'s "known remaining gap"
note is now closed — see that file's update.
