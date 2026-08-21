# `.^set_name` on a builtin type's `.WHAT` (`Hash`, `Array`, ...) is a no-op

## Status update (2026-08-21)

The narrow part of this ticket is fixed: `.^set_name` on a **user-declared
class's own `Package` value** now round-trips through `.^name` correctly
(`class Foo {}; Foo.^set_name("Foo(restricted)"); Foo.^name` now returns
`"Foo(restricted)"`, matching raku). See `t/classhow-set-name-package.t` and
`news/2026-08/set-name-package-round-trip.md`.

The root cause of the read-side gap: `dispatch_caret_name()`
(`src/runtime/methods_introspect.rs`), the fast path `.^name` actually uses
(a `meta_method != "name"` guard in `methods_call_dispatch.rs` routes plain
`.^name` around the generic `HOW` dispatcher entirely), never consulted
`type_metadata`'s `__set_name__` entry for a `Package` or `Instance` value —
even though `dispatch_classhow_method`'s `"set_name"`/`"name"` handlers
(reached only via explicit `.HOW.name(x)` calls) already read/write it
correctly. Fixed by making `dispatch_caret_name` consult the same
`type_metadata` map, and by guarding the *write* side (`"set_name"`'s
`ValueView::Package` arm) to refuse writing an override for a builtin type
name (`Interpreter::is_builtin_type`) — renaming the single shared `Package`
value that every value of a builtin type (e.g. every `Hash`) points to would
be an actively-wrong process-wide rename, not a scoped one. Verified: `Hash.^set_name(...)`
is now a safe no-op and does not affect unrelated hashes.

**What remains blocked**: the deeper issue this ticket originally flagged —
`Hash::Restricted`'s `v.var.WHAT.^set_name(...)` — is still unfixed. `%h.WHAT`
for a role-mixed hash returns the shared `Package("Hash")`
(confirmed: `%h.WHAT === Hash` is `True` in mutsu, `False` in real raku), not
a fresh per-composition anonymous type object the way Rakudo's real metamodel
gives one. An attempted fix (making a `Mixin` value's `.WHAT` reuse the
value's own `overrides` `Gc` handle) was tried and reverted in the same
session: it fixed `Hash::Restricted` but broke
`roast/S14-roles/instantiation.t` ("Punned role classes have the same
.WHAT") — two independently-`.new()`-ed instances of the same punned role
must return `===`-identical `.WHAT` objects, which an instance-keyed
`overrides` map cannot provide. The correct fix needs a
**composition-keyed** (base type + role set, not instance) anonymous type
object cache; that is written up as its own tracked item:
`todo/deep/mixin-what-identity-not-per-composition.md`. This ticket stays
open until that deeper item is resolved and `Hash::Restricted`'s 2 blocked
subtests ("is the name changed ok" x2) can be re-verified.

## Original symptom

```raku
class Foo {}
say Foo.^name;               # Foo
Foo.^set_name("Foo(restricted)");
say Foo.^name;                # mutsu (before fix): Foo (unchanged) -- raku: Foo(restricted)
```

(This exact repro is now fixed — see the status update above. The remainder
of this file is the original investigation writeup, kept for context.)

`methods_classhow_dispatch.rs`'s `"set_name"` handler (~line 190) DOES have a
branch for `ValueView::Package(name)` that writes into
`self.type_metadata.entry(name.resolve()).or_default().insert("__set_name__", ...)`
— so the write itself isn't silently dropped at that call site. The read
side (`.^name`) did not consult `__set_name__` for a `Package` value the way
it does for a `Mixin`'s `__mutsu_type_name__` override (see the same file,
the `ValueView::Mixin` branch just above) — confirmed root cause: `.^name`
routes through `dispatch_caret_name`, not `dispatch_classhow_method`, for the
plain (unqualified) `.^name` call form.

## Why this matters

`Hash::Restricted`'s custom `is restricted` trait calls
`v.var.WHAT.^set_name("$name(restricted)")` to give the restricted hash's
type a distinguishing display name (`%h.^name.ends-with('(restricted)')` is
one of its own test assertions). `v.var.WHAT` for a `%h` variable is a
`Package("Hash")` value (the shared builtin type), not a fresh per-instance
anonymous type the way Rakudo's actual metamodel gives a freshly role-mixed
object — so even if `.^set_name`/`.^name` round-tripped correctly here, it
would be renaming the SHARED global `Hash` type for every hash in the
program, not just this one (confirmed: real raku *does* rename globally if
you call `.^set_name` directly on the literal shared `Hash` package with no
role mixed in first — the scoping only works in real Rakudo because
`Hash::Restricted` mixes in a role *before* calling `.^set_name`, which gives
`.WHAT` a fresh per-composition type in real Rakudo, but not in mutsu).

## Discovered via

Investigating `todo/deep/trait-mod-does-not-callable-sub.md` (now resolved —
see `news/2026-08/trait-mod-does-callable-sub.md`), getting `Hash::Restricted`'s
32-subtest suite running as far as possible. Blocks 2 of its subtests
("is the name changed ok" ×2, one per `%h1`/`%h2` case) — cosmetic relative
to the dist's core restriction behavior, which does not depend on the name.

## Next steps

1. ~~Trace `.^name` for a `Package` value to confirm whether/why it ignores
   `type_metadata`'s `__set_name__` entry.~~ Done — see status update.
2. Pick up `todo/deep/mixin-what-identity-not-per-composition.md`: design a
   composition-keyed anonymous type object cache so a role-mixed native
   value's `.WHAT` returns a distinct, per-composition (not per-instance,
   not shared-base) type object. Once that lands, re-run this ticket's
   `Hash::Restricted` repro and close this ticket out to `news/` if the 2
   blocked subtests pass.
