# `42 but 'forty two'` is an anonymous-role mixin, not an `IntStr` allomorph

`(42 but 'forty two').^name` reported `IntStr`. Raku reports `Int+{<anon|1>}`, and — the part the
ticket did not record — such a value does **not** do `Str`: `(42 but 'forty two') ~~ Str` is
`False`, while the genuine allomorph `<42> ~~ Str` is `True`.

## What raku actually does (measured first)

`$obj but <some value>` composes a fresh *anonymous role* supplying one method named after the
value's type. Every `but`-with-a-value case behaves the same way and none of them acquires the
mixed value's type:

```
(42 but 'forty two').^name   # Int+{<anon|1>}   ~~ Str  -> False
(1 but True).^name           # Int+{<anon|2>}   ~~ Bool -> False
(1 but C.new).^name          # Int+{<anon|4>}   ~~ C    -> False
<42>.^name                   # IntStr           ~~ Str  -> True
(<42> but R).^name           # IntStr+{R}       ~~ Str  -> True
```

Two separately-composed `1 but "hi"` values are also distinct objects (`===` is `False`), because
each `but` mints its own anonymous role.

## Root cause

mutsu represents a value mixin as `Mixin(inner, { <value's type name> => value })` — which for a
`Str` right-hand side is byte-for-byte the shape a genuine allomorph uses
(`{"Str" => "42"}`). Nothing distinguished the two, so `allomorph_type_name` claimed every
`Int but <Str>` as an `IntStr`, and three further sites re-derived the same (wrong) conclusion
independently.

## Fix

`Interpreter::apply_single_mixin` (`src/vm/vm_mixin_does_ops.rs`) now records a
`__mutsu_value_mixin__` marker whose value is a freshly minted anonymous-role name (drawn from
the same counter the parser uses for a `role { }` literal, so ids stay unique within a process).
The marker does double duty: it names the composition for display, and its presence is what tells
a value mixin apart from an allomorph. It is deliberately *not* spelled as a `__mutsu_role__`
marker — those drive real role-method lookup, role-body composition, and several `.clone`/dispatch
gates, none of which apply to a role with no declaration behind it. (Spelling it as a role marker
was tried first and broke `(5 but False).clone`, whose fast path is gated on the absence of role
markers.)

Consumers updated to respect the marker:

- `allomorph_type_name` (`src/value/types.rs`) returns `None` for a value mixin — so `.^name`,
  `.WHAT`, `.WHICH`, and the gist all report the composition. `what_type_name` also learned to
  keep *both* halves when a genuine allomorph has a role composed onto it (`IntStr+{R}`).
- `role_mixin_suffix_excluding` renders the marker's anon name in the `+{...}` suffix.
- `Value::isa_or_does_check` (`src/value/types_isa.rs`) and `Interpreter::type_matches_value`
  (`src/runtime/types/type_matching.rs`) no longer treat the override key as a type the object
  does.
- `Interpreter::mixin_chain` (`src/runtime/receiver_class.rs`) held a *third*, independent copy
  of the allomorph test (`mixins.contains_key("Str")` plus a local inner-type match) that fed the
  dispatch MRO; it now delegates to `allomorph_type_name`, the single oracle. This one was the
  last holdout: with only the other three fixed, `42 but "forty two"` still smart-matched `Str`
  through its (wrongly `IntStr`) MRO chain.

Pinned by `t/role-mixin-survival.t`; `t/decl-mixin-begin.t`'s assertion that a value mixin is
"not role-suffixed" was re-measured against raku and corrected.
