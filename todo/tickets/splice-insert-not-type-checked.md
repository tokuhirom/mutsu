# `.splice` does not type-check a `Nil` replacement value against a declared element type

Originally filed as "`.splice`'s inserted replacement values are never type-checked at all" —
`do_splice` (`src/runtime/methods_mut_dispatch.rs`) built `new_items` from `args[2..]` with no
call to `check_container_element_types`, unlike `push`/`append`/`unshift`/`prepend`, which all
check before inserting. That general gap has since been closed. What survives is one hole in
the check plus a message-wording difference, so the ticket is rescoped.

## Repro

**Re-measured on `main` @ `17139dd55` against `raku` v2026.06 (2026-08-25): the
general case is now fixed; only the `Nil`/`Any` case is left.**

| repro | raku | mutsu |
| --- | --- | --- |
| `my Int @a = 1,2,3; @a.splice(1,0,"x")` | dies `X::TypeCheck::Splice`: "Type check failed in splice; expected Int but got Str (Str)" | dies `X::TypeCheck::Splice`: "Type check failed for an element of @a; expected Int but got Str" |
| `my Int @a = 1,2,3; @a.splice(1,0,Nil)` | dies `X::TypeCheck::Splice`: "Type check failed in splice; expected Int but got Any (Any)" | **no throw** — yields `Array[Int].new(1, Any, 2, 3)` |

So `.splice` *does* type-check its inserted values now, and it throws the same
exception class `raku` does. Two things are left:

1. **A `Nil` replacement argument still slips through.** ADR-0049 decays a spliced-in
   `Nil` to plain `Any` (correct — `raku` does not use the target's `is default(...)`
   for splice, unlike push/append/unshift/prepend), but the decayed `Any` is then
   inserted into an `Array[Int]` without being checked against the element type.
   `raku` rejects it. The check evidently runs before the decay, or skips
   already-`Any` values, so the decay output never reaches it.
2. **The message text differs.** mutsu says "Type check failed for an element of @a;
   expected Int but got Str" — the generic element-store wording — where `raku` says
   "Type check failed in splice; expected Int but got Str (Str)", naming the operation
   and repeating the value's type in parentheses. Same exception class, so this only
   matters to code matching on `.message`.

## Where

`do_splice` (nested fn inside the `"splice" =>` arm,
`src/runtime/methods_mut_dispatch.rs`, around line 1029) builds `new_items`
from `args[2..]` with no type check. The sibling arms (`push` at ~:758,
`append` at ~:778, `unshift` at ~:829, `prepend` at ~:871) all call
`self.check_container_element_types(&key, &target, &values)?` before
inserting.

## Why this is separate from ADR-0049

ADR-0049 (Nil decays to the container default at the element store) fixed
`.splice`'s `Nil`-specific handling: a `Nil` replacement arg now decays to
plain `Any` (matching real raku, which — unlike push/append/unshift/prepend
— does NOT use the target's `is default(...)` value for a spliced-in `Nil`;
verified against `raku -e`). That is a narrow, Nil-only fix. This ticket is
the broader, pre-existing gap: `.splice` never type-checks ANY inserted
value (not just a decayed `Nil`/`Any`), which is a general correctness bug
independent of Nil handling.

## Fix sketch

The check now exists, so the work is to find why the `Nil` path bypasses it. Locate the
`check_container_element_types` call that the `"splice"` arm gained, and determine its order
relative to ADR-0049's `Nil` → `Any` decay: if the check runs on the pre-decay values, a `Nil`
looks like a legitimate "reset to default" marker and passes, and the `Any` it becomes is never
re-checked. Moving the check to run on the post-decay `new_items` is the likely one-line fix,
but confirm the ordering under `rust-gdb` rather than assuming it.

Then align the message with `raku`'s "Type check failed in splice; expected Int but got Str
(Str)" wording — mutsu currently reuses the generic element-store message. Both `raku` shapes
name the operation, so a splice-specific message string is needed rather than the shared one.

Pin both with a `t/` test covering: a wrong-typed non-`Nil` insert (already passing), a `Nil`
insert into a typed array (currently silent), a `Nil` insert into an *untyped* array (must
still be allowed), and a splice with no replacement values at all.
