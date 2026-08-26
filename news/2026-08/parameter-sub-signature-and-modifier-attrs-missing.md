# `Parameter.sub_signature` and `Parameter.modifier` are implemented

`$sig.params[0].sub_signature` and `$sig.params[0].modifier` both died with
"No such method … for invocant of type 'Parameter'".

## Root cause

`Parameter` reflection objects are `Value::Instance`s whose attribute map is
built by `build_parameter_attrs()` in `src/value/signature.rs`; because
`Parameter` is a builtin rather than a user-declared class, method dispatch
auto-generates a zero-arg accessor for every key in that map. So the two
methods were missing purely because the two keys were never populated.

- **`.modifier`** — mutsu keeps a type-definedness smiley on the
  type-constraint *string* (`Str:U`, `UInt:D`), so the modifier is read back
  off a trailing `:U` / `:D` / `:_` and answers `""` otherwise. This reads only
  a trailing smiley, so a coercion type such as `Int(Str)` is unaffected.
- **`.sub_signature`** — the parameter's `sub_signature` params are
  materialized into a real `Signature` value; a parameter without one answers
  the undefined `Signature` type object, which is what makes rakudo print
  `(Signature)`.

One subtlety the ticket did not mention: mutsu records a **named alias chain**
(`:s(:$sort)`) in the *same* `sub_signature` slot as a destructuring
sub-signature. Rakudo reports `(Signature)` for an alias — it is not a
destructure — so an all-named sub-signature under a named parameter is
deliberately excluded, and `:(:s(:$sort)).params[0].sub_signature` is undefined
in mutsu too. The pre-existing `sub-signature` (hyphenated) attribute, which
answers an array of `Parameter`s, is untouched.

Pinned by `t/signature-binding-gaps.t`.
