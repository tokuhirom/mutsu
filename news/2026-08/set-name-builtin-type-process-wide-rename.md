# `Foo.^set_name` on a builtin type now genuinely renames it process-wide, matching Rakudo

A follow-up to `news/2026-08/set-name-package-round-trip.md`: that fix made `.^set_name` round-trip
through `.^name` for a user-declared class's own `Package` type object, and — as a safety
measure — made `.^set_name` a silent no-op when called on a *builtin* type's shared `Package` value
(`Hash`, `Array`, `Int`, ...), on the theory that renaming the single object every value of that
type points to would be an actively-wrong process-wide rename rather than a scoped one.

Checking that theory against real `raku` showed it was backwards:

```raku
my %h;
Hash.^set_name("Hash(renamed)");
say Hash.^name;   # Hash(renamed)
say %h.^name;     # Hash(renamed) -- Rakudo really does rename it globally
```

Rakudo's `Hash.^set_name` genuinely renames the single shared metaobject process-wide — there is no
scoping, because a builtin type's `Package` object really is one global object. The mutsu no-op
guard was solving a problem Rakudo itself does not solve; the previous session's `Hash::Restricted`
investigation needed a *scoped* rename, but the right tool for that turned out to be
[ADR-0060](../../docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md)'s
composition-keyed anonymous type object (landed separately, see
`news/2026-08/mixin-what-composition-keyed-identity.md`), not a special case on the shared builtin
`Package`.

Fixed by removing the `is_builtin_type` write-side guard in `dispatch_classhow_method`'s
`"set_name"` handler (`src/runtime/methods_classhow_dispatch.rs`) — a builtin `Package` value now
writes its `__set_name__` override into `type_metadata` exactly like a user class's does. That alone
only fixed `Hash.^name`/`Hash.HOW.name(Hash)` (the type object itself); a *concrete value* of a
renamed builtin (`%h.^name`, `5.^name`) still reported the old name, because `dispatch_caret_name`'s
and `dispatch_classhow_method`'s fallback arms for a plain `ValueView::Hash`/`Int`/etc. resolved the
name via `value_type_name`/`dispatch_owner_name` directly, never consulting `type_metadata` at all.
Added a shared helper, `Interpreter::builtin_display_name` (`src/runtime/methods_introspect.rs`),
that both fallback arms now route through, so every read path — plain `.^name`, `.HOW.name(...)`,
and a concrete value of the renamed type — agrees.

Verified against `raku` for both `Hash` and `Int`, and against
`roast/S14-roles/instantiation.t` (all 19 subtests, the punned-role `.WHAT` identity invariant
`.^set_name` on a builtin must not disturb) and `roast/S12-meta/primitives.t` (the one whitelisted
roast test exercising `.^set_name` directly). New coverage in `t/classhow-set-name-package.t`
(updated to assert the process-wide rename instead of the no-op it previously encoded, plus new
assertions for `.HOW.name` on a value and for a non-Hash builtin type).

Originally investigated as `todo/tickets/set-name-on-builtin-type-package-no-op.md`; closed out
now that the read-side gap that ticket flagged, and the deeper `Hash::Restricted`
`v.var.WHAT.^set_name(...)` composition-identity blocker it depended on, are both resolved.
