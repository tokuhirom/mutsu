# `class Foo does Dateish { ... }` rejected — `Dateish` is not a registered composable role

## Symptom

```raku
class Time::Local does Dateish {
    has $.hour;
}
say "ok";
```

Under `raku`: `ok` (Raku's `Dateish` is a real role — `Date` and `DateTime`
both `does Dateish` themselves, and it can be composed into a user class).
Under mutsu (`target/debug/mutsu`):

```
X::InvalidType: Invalid typename 'Dateish'
```

## Root cause

`src/runtime/registration_class_decl.rs`'s `BUILTIN_PARENT_TYPES` list already
contains `"Date"` and `"DateTime"` (so `class Date::Local is Date {}` — real
inheritance — works), but **not** `"Dateish"`. The validation error comes from
`src/runtime/registration_class_validate.rs` (~line 228-239): a `does` parent
is checked against `self.registry().classes`, `BUILTIN_TYPES` (=
`BUILTIN_PARENT_TYPES`), `self.registry().roles`, and `self.registry().enum_types`
— `Dateish` matches none of these, so it hits the `X::InvalidType` branch.

Note `Dateish` **is** already recognized in several other places as an isa-check
target (it is not a total unknown to the interpreter):

- `src/value/types_isa.rs:267` — `"Dateish" => matches!(...)` for `Date`/`DateTime`
- `src/runtime/utils/type_constraints.rs:202`, `src/vm/vm_value_helpers.rs:398`,
  `src/vm/vm_misc_ops.rs:75` — treated as a valid type-constraint name
- `src/runtime/methods_instance_ops.rs:2667` — part of a method-dispatch
  fallback chain (`["Dateish", "Real", "Numeric", "Cool", "Any"]`)

So the gap is narrow: `Dateish` is wired in as a *recognized type name* for
matching/dispatch purposes, but not as an actual **composable role** a
user-defined class can `does`. Fixing this needs two parts:

1. Add `"Dateish"` to `BUILTIN_PARENT_TYPES` (or the equivalent role-registry
   check) so `does Dateish` passes class validation.
2. Decide what a `does Dateish`-composed class actually gets: real Raku's
   `Dateish` role declares required methods (`daycount`, `year`, `month`,
   `day`, `formatter`, `truncated-to`, `later`, `earlier`, ...) that the
   composing class must supply (or that have defaults building on
   `daycount`). Check whether TOML::Thumb's `Time::Local` (which only
   implements `!formatter` — a *private* method, likely not what the role
   requires) actually satisfies the real role's contract in Rakudo, or
   whether Raku's role-composition rules are lenient here for some other
   reason, before assuming a minimal stub registration is sufficient.

## Why this matters

Found while surveying candidates for mutsu's TOML-parser battery slot
(`docs/batteries/toml.md`, 2026-08-22). `TOML::Thumb` (`zef:JRaspass`, MIT,
zero runtime deps) was runner-up to the winning `Config::TOML` candidate —
it is small, well-scoped, and its own upstream suite is otherwise clean
(18 known gaps are explicitly `# TODO` and non-fatal). This one role-
registration gap is the **entire** blocker for its whole test suite
(`invalid.t`, `valid.t`) loading at all under mutsu:

```
X::InvalidType: Invalid typename 'Dateish'
  in block <unit> at ./TOML/Thumb.rakumod line 7
```

This is also a general gap (not TOML::Thumb-specific): any module that
defines its own `Dateish`-compatible date/time-of-day type hits the same
wall, since `Dateish` is Raku's documented public extension point for
"acts like a date" (see `raku-doc/doc/Type/Dateish.rakudoc` if present, or
the upstream Rakudo source for the role's actual method contract).

## Next steps

1. Read `Dateish`'s real method contract (Rakudo source or
   `raku-doc/doc/Type/Dateish.rakudoc`) to know exactly what a composing
   class must supply.
2. Register `Dateish` as a real composable role (not just a matched type
   name) with that contract, defaulting derived methods (`Str`, comparison
   operators, etc.) the way `Date`/`DateTime` already do internally.
3. Re-run `TOML::Thumb`'s two upstream test files
   (`tmp/toml-survey/toml-thumb/{valid,invalid}.t` if the scratch survey
   directory still exists, or re-fetch per `docs/batteries/toml.md`) to see
   how far this one fix takes the suite.
