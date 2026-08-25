# `Dateish` is now a real composable role

`class Foo does Dateish { ... }` used to fail with `X::InvalidType: Invalid
typename 'Dateish'`, even though `Dateish` was already recognized in several
other places (isa/smartmatch checks, `.^methods` fallback chains) as a valid
type name. The gap was narrow: `Dateish` was wired in as a type-name that
mutsu could *match against*, but it had never been registered as an actual
composable role a user-defined class could `does`.

Real Raku's `Dateish` role is what both `Date` and `DateTime` themselves
compose, and it is documented as a public extension point for any type that
"acts like a date" (`raku-doc/doc/Type/Dateish.rakudoc`). Its default `Str`
does not carry its own formatting logic; instead it dispatches to a private
`self!formatter()` that the composing class is expected to supply (`Date` and
`DateTime` each implement their own `!formatter` privately). A composing
class that omits `!formatter` gets Rakudo's own "No such private method
'formatter'" error when stringified — there is no silent generic fallback.

mutsu now mirrors this narrow, well-defined contract:

- `"Dateish"` was added to `BUILTIN_PARENT_TYPES`
  (`src/runtime/registration_class_decl.rs`), so `does Dateish` passes class
  validation the same way `does Date`/`does DateTime` already did.
- `call_method_with_values()` (`src/runtime/methods_call_dispatch.rs`) gained
  a default `.Str`/`.gist`/`.Stringy` handler for any instance whose class
  composes `Dateish` (via `class_does_role`) and does not define its own
  override: it dispatches to the instance's private `!formatter`, simulating
  the call as though it originated from a method owned by that class (so the
  private-method-permission check accepts it, exactly as it would for a real
  `self!formatter()` call inside the role's own `Str` body). `.Stringy` had
  to be included alongside `.Str`/`.gist` because `~$obj` and string
  interpolation try `.Stringy` first, and a generic instance already answers
  that with a "ClassName()" default that would otherwise win before `.Str`
  was ever tried.

This was found while surveying TOML-parser candidates for mutsu's TOML
battery slot (`docs/batteries/toml.md`): `TOML::Thumb`'s `Time::Local`
(`class Time::Local does Dateish { has $.hour; ... }`) hit exactly this wall,
and the composability gap was the entire blocker for its whole test suite
loading at all. With the fix, `TOML::Thumb`'s `t/invalid.t` now passes
cleanly under mutsu, and `t/valid.t` progresses well past module load into an
unrelated, separate gap (a recursive `.&?BLOCK` block-argument call in the
test file itself, tracked as a distinct issue if `TOML::Thumb` is revisited).

Scope note: this deliberately implements only the part of `Dateish`'s
contract that real Raku itself treats as generic and role-level (the private
`!formatter`-backed `Str`/`gist`/`Stringy` default). Real Raku's `Dateish`
role additionally owns its own `$!year`/`$!month`/`$!day` state and a large
family of calendrical derived methods (`day-of-week`, `is-leap-year`,
`week-number`, `earlier`/`later`, ...) built on top of that state — that is
effectively a full reimplementation of `Date`/`DateTime`'s own internals as
generic, attribute-owning role machinery, and neither `Time::Local` nor any
other known consumer needs it. It was left out of scope; see
`src/builtins/methods_0arg/temporal_dispatch.rs` for the exact math those
derived methods would need to share if a future consumer requires them.

Regression test: `t/dateish-role-composition.t` (composability, `.does`,
smartmatch, `.Str`/`.gist`/`~`/interpolation via `!formatter`, `Date`/
`DateTime` still composing `Dateish`, and a plain non-`Dateish` class staying
unaffected).
