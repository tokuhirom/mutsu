# Nested multi `EXPORT` hooks are scoped per compilation unit

Red 0.2.5 exposed a remaining variant of the nested custom-`EXPORT` bug. A
module with `multi EXPORT(+@args)` could `use` a module with an ordinary
`sub EXPORT`, but mutsu treated the outer hook's arity-suffixed registry entry
(`GLOBAL::EXPORT/0`) as an ordinary routine. The inner module then failed with
`Redeclaration of routine 'EXPORT'`, even though Raku scopes each hook to its
own compilation unit.

The per-compilation-unit hide/restore path now recognises arity-suffixed
`EXPORT` entries, and custom hooks dispatch through the actual `use` arguments
when the hook is a multi. The existing nested-export regression now covers the
multi form as well. This lets Red progress past its `EXPORT` root cause; its
remaining role-composition failures are independent.
