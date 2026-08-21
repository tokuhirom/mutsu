# UUID battery bundled

`UUID` (`github:retupmoca`, v1.0.0, MIT) is now bundled — `use UUID;` resolves
zero-config, no `zef install` needed. Chosen over `UUID::V4` and `LibUUID` on
ecosystem standing (30 dependents vs. 0 and 3); see
[docs/batteries/uuid.md](../../docs/batteries/uuid.md) for the full field
survey.

Bundling it surfaced a general interpreter bug: a user class's own `has
$.bytes` accessor was silently shadowed by mutsu's native `.bytes` builtin
(`UUID`'s only attribute is `has $.bytes`, holding its 16-byte `buf8`).
`.bytes` is a Cool-only builtin in real Rakudo — a plain `Any`-derived class
doesn't resolve it at all — and every other Cool-only name already deferred
to the interpreter (which prefers a class's own accessor) via
`cool_only_builtin_method()`, but `bytes` had been left off that list. Every
class declaring `has $.bytes` was affected, not just this module; fixed by
adding `"bytes"` next to `"chars"`/`"codes"` in the gate
(`src/runtime/methods_native_bypass.rs`). Pin:
`t/bytes-attribute-accessor-not-shadowed.t`.
