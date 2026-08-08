# A user `trait_mod:<is>` multi typed `(Method $m, ...)` never dispatches

## Root cause

`class_body_method_decl` (and, as of ADR-0019 D3-6, `augment_class`'s method
arm) apply a user-defined custom trait (`method foo() is loud { ... }`) by
building a code-object value for the about-to-be-installed method with
`Value::make_sub(...)` and passing it as the first argument to
`self.call_function("trait_mod:<is>", args)`. `Value::make_sub` always
produces a plain `Value::Sub` — there is no `is_method` flag or equivalent on
`SubData` that marks a code object as a `Method` rather than a `Sub`.

Real Raku modules that hook `is <trait>` on methods declare the multi
candidate typed against `Method`, e.g.:

```raku
multi sub trait_mod:<is>(Method $m, :$loud!) {
    say "custom trait applied to {$m.name}";
}
```

Since mutsu's `sub_val` reports as a `Sub`, not a `Method`, this candidate's
`Method $m` parameter type-checks the argument and rejects it — no candidate
matches, `call_function` returns an error, and the call site discards it with
`let _ = self.call_function(...)`, so the trait application silently does
nothing. Confirmed against `raku`: the same script prints
`custom trait applied to greet`; mutsu prints nothing.

An *untyped* candidate (`multi sub trait_mod:<is>($m, :$loud!)`) does not fix
it either — that is not a workaround, because `raku` itself refuses to compile
that form for a method-level trait (`Can't use unknown trait 'is' -> 'loud' in
method declaration`), so an untyped candidate is not what real modules ship.

## Why this is deep / broad

Fixing this needs a real way to answer "is this code object a `Method`"
consistent with the rest of the type system (`.WHAT`, `.isa(Method)`,
signature type-checking) — not just a local hack at the `trait_mod:<is>` call
site. `Value::Sub`/`SubData` has no such marker today, and adding one touches
whatever currently decides a code object's reported type (`.^name`, `isa`
checks, dispatch). The three call sites that build this `sub_val`
(`registration_class_body_method.rs`, `registration_role_method.rs`,
`registration_class_augment.rs` after ADR-0019 D3-6) are all equally affected
— this is not walker drift, it is one shared, pre-existing bug.

## Affected files

- `src/runtime/registration_class_body_method.rs` (the `sub_val` construction
  and the two `call_function("trait_mod:<is>", ...)` call sites)
- `src/runtime/registration_role_method.rs` (same shape)
- `src/runtime/registration_class_augment.rs` (same shape, added by D3-6)
- `src/value/value_methods_b.rs` (`Value::make_sub`, `SubData` — the type
  representation that would need the fix)

## Minimal repro

```raku
multi sub trait_mod:<is>(Method $m, :$loud!) {
    say "custom trait applied to {$m.name}";
}
class Foo {
    method greet() is loud { "hi" }
}
```

`raku` prints `custom trait applied to greet`; mutsu (as of 2026-08-08) prints
nothing, on `main` and after ADR-0019 D3-6 alike.
