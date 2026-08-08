# A user `trait_mod:<is>` multi typed `(Method $m, ...)` now dispatches

A user-defined custom trait on a method (`method foo() is loud { ... }`)
never actually invoked the user's `trait_mod:<is>` handler when that handler
was typed against `Method`, e.g.:

```raku
multi sub trait_mod:<is>(Method $m, :$loud!) {
    say "custom trait applied to {$m.name}";
}
class Foo {
    method greet() is loud { "hi" }
}
```

`raku` prints `custom trait applied to greet`; mutsu printed nothing. Real
Raku modules always type the candidate against `Method` — `raku` itself
rejects an untyped `$m` at the method-declaration site ("Can't use unknown
trait") — so this was a silent, system-wide no-op for any such module.

The root cause: the code object built for the about-to-be-installed method
(passed as the multi candidate's first argument) was a plain `Value::Sub`.
`Value::Sub`'s type-check machinery already knows how to report a code object
as `Method` instead of `Sub` — `sub_value_from_function_def` sets a
`__mutsu_callable_type = "Method"` marker in the captured env for a real
method's code object, and `isa`/type-check reads it — but the two sites that
build a *transient* code object purely to pass to `trait_mod:<is>`
(`class_body_method_decl`'s custom-trait loop, and `augment_class`'s
equivalent added by ADR-0019 D3-6) never set that marker, so the `Method $m`
parameter never type-checked and no candidate matched.

Both sites now set the same marker. The role walker still has no
`trait_mod:<is>`/`is export` handling on methods at all (a separate,
pre-existing gap noted at ADR-0019 D3-3, not touched here).

Verified against `raku`. Full `t/` suite (27901 tests) plus the
`S06-traits`/`S06-multi` roast whitelist all green.
