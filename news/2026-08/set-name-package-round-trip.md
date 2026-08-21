# `.^set_name` on a user-declared class's own type object now round-trips through `.^name`

`Foo.^set_name("newname")` followed by `Foo.^name` used to silently show the
old name, even though the write itself was landing correctly:
`dispatch_classhow_method`'s `"set_name"` handler
(`src/runtime/methods_classhow_dispatch.rs`) already persisted the override
into `type_metadata` for a `ValueView::Package` value. The bug was on the
read side: plain `.^name` does not go through `dispatch_classhow_method` at
all — `methods_call_dispatch.rs` special-cases `^name` to bypass the generic
`HOW` dispatcher and go straight to `dispatch_caret_name()`
(`src/runtime/methods_introspect.rs`), which never looked at
`type_metadata`'s `__set_name__` entry for a `Package` or `Instance` value,
and never checked a `Mixin` value's `__mutsu_type_name__` override either
(despite `dispatch_classhow_method` already having that exact check for the
`.HOW.name(x)` call form).

Fixed by making `dispatch_caret_name` consult the same `type_metadata` map
(and the same `Mixin` override) that `dispatch_classhow_method` already
reads. The write side got a matching safety guard: renaming a *builtin*
type's `Package` value (`Hash`, `Array`, ...) is now a no-op rather than a
write, because that `Package` value is the single shared object every value
of that type points to — renaming it would rename the type process-wide for
every `Hash` in the program, not just the caller's. Verified directly:
`Hash.^set_name(...)` no longer affects an unrelated `%h.^name`.

This does not yet fix the harder case the investigation started from —
`Hash::Restricted`'s `v.var.WHAT.^set_name(...)`, where `.WHAT` on a
role-mixed hash still returns the shared `Hash` package rather than a
distinct per-composition type object. That remains open as
`todo/deep/mixin-what-identity-not-per-composition.md`.

New regression coverage: `t/classhow-set-name-package.t`.
