# `Parameter` gains `constraint_list`, `usage-name`, and a defined `default`

Three `Parameter` introspection methods used by Cro's HTTP router
(`Cro::HTTP::Router::LinkGenerator::signature-to-sub`) were missing or
incomplete:

- `.constraint_list` — died with "No such method" (only `.constraints`,
  the `all(...)` junction of the same items, existed).
- `.usage-name` — died with "No such method" (the variable name minus
  sigil and twigil).
- `.default` — died with "No such method" when the parameter had no
  default at all, instead of returning the undefined `Code` type object.

## Root cause

`build_parameter_attrs` (`src/value/signature.rs`) populates the
`Parameter` instance's attribute map, and a built-in class like
`Parameter` answers a zero-arg method call by looking the method name up
directly in that map (`methods_instance_ops.rs`'s "auto-generated
accessor for public attributes" fallback) — so a missing attrs entry
surfaces as `No such method`, not as an undefined value. `constraint_list`
was never inserted (only the `all()` junction built from the same items),
`usage-name` was never computed, and `default` was only inserted when a
default expression was actually present.

## Fix

In `build_parameter_attrs`:
- Insert `constraint_list` (the raw `Vec<Value>`, before it's consumed
  into the `all()` junction) as an Array.
- Compute `usage-name` by stripping the twigil (already-known via
  `extract_twigil`) off the bare parameter name.
- Always insert `default`, using `Value::Package(Symbol::intern("Code"))`
  as the undefined type object when no default expression exists.

## Verification

- The three repro one-liners from the diagnosis match raku exactly,
  including `constraint_list.raku` rendering as `("x",)` and `default`
  answering `Code`/undefined when absent.
- `with $param.default` and `$p.constraint_list == 1 && $p.constraint_list[0]
  ~~ Str` (the exact Cro idioms) now behave like raku.
- `t/http-router-named-urls.t` (vendored Cro::HTTP suite) progresses from
  dying immediately in `signature-to-sub` to emitting real TAP output.
- New pin: `t/parameter-introspection.t` (passes under both `mutsu` and
  `raku`).
- `roast/S06-signature/introspection.t` (154 subtests) and the full
  `S06-signature`/slurpy roast sweep (43 files, 1071 subtests) pass with
  no regressions.
