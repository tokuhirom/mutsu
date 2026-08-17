# callsame/nextsame from a gist/Str/raku override now reaches the native Mu default

`method gist() { "custom+" ~ callsame }` (and the same shape for `raku()`,
`Str()`, a role-composed override, or a `multi method gist()`) used to warn
"Use of Nil in string context" and drop the base rendering: `callsame`
returned `Nil` instead of Mu's native `ClassName.new(...)` default.

## Root cause

A single (non-multi, non-wrapped) compiled method dispatch
(`call_compiled_method`/`call_compiled_method_fast` in
`src/vm/vm_method_dispatch.rs`) pushed `method_class_stack` but never a
`SamewithContext`, so `callsame`/`nextsame`'s exhausted-MRO fallback
(`dispatch_next_candidate` in `src/runtime/builtins_dispatch_next.rs`), which
keys off `samewith_context_stack.last()` to learn "what method is currently
executing," found nothing and silently returned `Nil`. The equivalent `new()`
fallback worked because constructor dispatch goes through a different,
slower path that always pushes a `SamewithContext` (fixed earlier — see
`news/2026-08/callsame-native-mu-new-fallback.md`); this covers the
remaining `gist`/`Str`/`raku` case.

## Fix

- Added a `uses_dispatcher` compile-time flag on `CompiledCode`
  (`src/opcode.rs`), set during `emit()` when the body directly calls
  `callsame`/`nextsame`/`callwith`/`nextwith` — either as a call opcode, or
  (the common no-parens case) as a bareword term read (`GetBareWord`).
- Gated a `push_method_samewith_context`/`pop_method_samewith_context` pair
  on that flag at all 8 entry/exit points `call_compiled_method`/
  `call_compiled_method_fast` already bracket with
  `push_method_class`/`pop_method_class`, so the overwhelming majority of
  method calls (which never call `callsame`) pay no extra clone.
- Added `native_any_base_next_candidate` (mirrors
  `native_array_storage_next_candidate`'s same no-frame shape) to dispatch
  `gist`/`Str`/`raku` to Mu's native default once the context is available.
- Discovered along the way that the native fast dispatch (`try_native_method`)
  has no rendering for a plain Instance's `gist`/`raku` at all — that logic
  lives in the slow interpreter path (`dispatch_instance_and_fallback`),
  gated behind `!has_user_method(...)`. Extracted it into a shared
  `default_instance_repr` (`src/runtime/methods_instance_ops.rs`) so both the
  original no-override site and the new callsame fallback render identically.
  `.Str` has no comparable default method in Rakudo (its own default is an
  identity-ish `ClassName<objectid>`, not reproducible), so that case reuses
  the same generic `ClassName()` fallback `Value`'s `Display` impl already
  produces for an instance with no better answer.

Pinned by `t/gist-str-raku-callsame-native-mu-fallback.t` (plain override,
`raku()`, role-composed override, multi method) — all four match Rakudo
v2026.06 exactly.
