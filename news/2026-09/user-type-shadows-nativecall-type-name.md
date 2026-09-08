# A user type named `void`/`long`/`size_t` reports its own name

```raku
class void { }
say void.^name;
# raku: void   mutsu: NativeCall::Types::void
```

No `use NativeCall` was required for this. `.^name` is load-bearing in every
`X::` message and in `.raku`/`.gist`, so a user type that happened to be called
`long` rendered under a package it was never declared in — a wrong answer about
user-declared code, produced by machinery for a module the program does not use.

## Root cause

[ADR-0056](../../docs/adr/0056-nativecall-types-display-only-qualification.md)
renders NativeCall's builtin types under their real `NativeCall::Types::`
package as a **display-only** qualification: the registry key stays bare, and
one helper in `src/value/display.rs` qualifies the name for a human. That helper
is a pure function with no interpreter context, so its decision was keyed purely
on the bare name — and a user's own type of that name matched it.

`int8`/`short` were the discriminator the ticket named: they are *core* native
types, already correct, and took a different path.

## The fix

The four declaration ops (`class`/`grammar`, `role`, `subset`, `enum`) now call
`note_user_declared_type_name` with the **source-written** name, which sets one
bit of a process-global `AtomicU16` when that name collides with a
`NATIVECALL_TYPE_NAMES` entry; the qualifier stands down for a name whose bit is
set. Passing the source-written name is what keeps NativeCall's own prelude out
of the set by construction — it spells its types `class GLOBAL::Pointer` and
`class GLOBAL::void`, while a user writes them bare. A nested
`module M { class void { } }` registers `M::void` and never collided in the
first place.

Ten names fit a `u16`, so a declaration costs one relaxed `fetch_or` and a
render one relaxed load; the mask is zero in every program that does not declare
one of these names. It stays out of the interpreter for the same reason ADR-0056
put the qualification in `display.rs`.

Pinned by `t/user-type-shadows-nativecall-type-name.t`, whose 17 assertions pass
unchanged under rakudo: every colliding name as a class, the `int8`/`short`
controls, `role`/`grammar`/`subset`/`enum`, `.raku`/`.gist`, and a nested
declaration. The seven existing NativeCall pins — including
`t/nativecall-type-surface.t`, which asserts the qualified spelling for real
`use NativeCall` code — are unmoved.

Closes #7582.
