# NativeCall's `Pointer`/`CArray`/`long`/... now report their real `NativeCall::Types::*` name

Real Rakudo registers NativeCall's `Pointer`, `CArray`, `void`, `OpaquePointer`, and the
seven C-width integer aliases (`long`, `ulong`, `longlong`, `ulonglong`, `size_t`,
`ssize_t`, `bool`) under the `NativeCall::Types` package, so `.^name` reports the fully
qualified path (`NativeCall::Types::Pointer`, not `Pointer`). mutsu reported the bare short
name for all ten, and had a standing self-inconsistency on top of that: an *instance*'s
`.gist` already hardcoded the qualified spelling (`NativeCall::Types::Pointer<NULL>`) while
the *type object*'s `.^name` reported the bare one.

An earlier investigation (`todo/deep/nativecall-types-package-qualification.md`, two
research passes) found the "obviously correct" fix — actually registering these classes
under their qualified key — has a real correctness trap: `Pointer[T]`/`CArray[T]`
parametrization is evaluated at runtime by re-stringifying the *already-resolved* symbol,
so qualifying the registry key would make ordinary, everyday `use NativeCall;
Pointer[uint8]` code — used pervasively in `t/` and by the real OpenSSL/`IO::Socket::SSL`
battery binding — silently start evaluating to `NativeCall::Types::Pointer[uint8]`, which
roughly fifteen exact-string-match call sites (parametric-type allow-lists, native-int
width/signedness tables, alias resolution) do not `::`-strip before comparing.
[ADR-0056](../../docs/adr/0056-nativecall-types-display-only-qualification.md) re-verified
that trap still reproduces on current `main` and records the decision: keep every registry
key bare, and add exactly one shared display-qualification helper
(`qualify_nativecall_type_name` in `src/value/display.rs`, feeding into the existing
`user_facing_type_name`) called only from the places that stringify a type name for a
human — `.^name`, `.raku`, and error-message type naming (`what_type_name`) — never from
identity/dispatch comparison sites.

`OpaquePointer === Pointer` identity was verified to still hold (it is a `constant` alias,
not a distinct registry key, so it was never at risk). Five `t/` assertions that hard-pinned
the bare spelling were updated to the qualified one, matching real Rakudo:
`t/nativecall-type-surface.t` and `t/nativecall-pointer.t`.

See [ADR-0056](../../docs/adr/0056-nativecall-types-display-only-qualification.md) for the
full investigation, the rejected Strategy 2 (a real qualified registry key), and what was
deliberately left out of scope (the `Pointer.new.gist` prelude method still does not embed
its `[T]` parametrization — a separate, pre-existing gap).
