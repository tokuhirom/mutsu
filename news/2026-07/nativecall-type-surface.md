# NativeCall's type surface is complete, and `explicitly-manage` / `refresh` exist

`use NativeCall` exports a fixed set of type objects and helper routines. An
inspection of that surface against Rakudo v2026.06 (recorded in
`todo/tickets/nativecall-surface-gaps.md`) found five holes; this closes all of
them, and a sixth bug the tests for them turned up.

## The missing type objects

`bool`, `ssize_t` and `OpaquePointer` were simply not declared. Naming one as a
term degraded to `Str` — what an undeclared bareword becomes — so `nativesizeof(bool)`
saw a bare string and `$x ~~ OpaquePointer` compared against the wrong type.
`void` *was* declared, but the prelude carrying it was injected only when the
source also contained the word `Pointer`, so `use NativeCall; say void.^name`
did not see it either: one of the four names gated all of them.

- **`bool`** is C's `_Bool`: one byte wide, and *signed*. Rakudo answers `-1`
  for `my bool $x = -1` and `44` for `= 300`, i.e. exactly `int8`, and it is an
  integer type there too — a native `bool` return boxes to `Int`, not to `Bool`.
  It is now a native integer type of that shape, marshalled through the same ABI
  slot as `int8`, and `CArray[bool]` round-trips a signed byte.
- **`ssize_t`** is the signed counterpart of `size_t`, 64-bit on every platform
  mutsu targets.
- **`OpaquePointer`** is NativeCall's historical spelling of `Pointer`, and an
  *alias* rather than a subclass — `OpaquePointer === Pointer` is True in Rakudo.
  A `constant` in the prelude is what preserves that identity; a class of its own
  would not. It was already accepted in a *signature* (`CType::from_type_name`
  maps it), so only its use as a term was missing.
- The prelude gate is now `use NativeCall` alone, so all four type objects
  arrive together.

## `explicitly-manage` and `refresh`

Both are NativeCall exports rather than builtins, so — like `cglobal` before
them — the user-visible sub is Raku in the injected prelude and only the
primitive underneath it is native.

`explicitly-manage($str)` hands a string's C buffer to the callee for good. A
plain `Str` argument is marshalled into a temporary `char*` that dies with the
call, which is right for a callee that copies the string and wrong for one that
*retains* the pointer; `Language/nativecall.rakudoc`'s `set_version` example
segfaults on its second call for exactly this reason. mutsu now returns a
`NativeCall::CStr` (Rakudo's own name for the object) carrying the address of a
deliberately leaked NUL-terminated buffer, and the `Str` parameter marshaller
hands C that stable address instead of a temporary. The documented `:$encoding`
is honoured by construction: the prelude calls `Str.encode($encoding)` and the
native half only takes the bytes.

`refresh($obj)` re-reads a CStruct's fields after C wrote them behind the
runtime's back. In mutsu there is nothing to re-read — a CStruct instance holds
only the C address, and every field access reads through it — so this is a
genuine no-op that returns 1, as Rakudo's `sub refresh($obj --> 1)` does. It
still has to exist, because bindings call it.

## Two general bugs the tests found

**Every lowercase return type on a sub returned `Nil`.** The compiler decides
whether `--> spec` names a *definite return value* (a literal or lowercase term
the sub returns regardless of its body, as in Rakudo's own `--> 1`) or a return
*type*, and it decided on the first character alone. Raku's native types are
lowercase too, so `sub f($x --> ulong) { $x }` was read as "return the term
`ulong`", sank the body and answered `Nil` — for every one of `int`, `num`,
`str` and the `NativeCall::Types` C-width aliases. Native type names are now
excluded from that heuristic. (Methods were unaffected, which is why
`t/native-c-width-int-types.t`'s `method span(ulong $extra --> ulong)` passed
while the sub form never had.)

**Every `Bool` argument reached C as 0.** `Bool` unboxes to 1/0 in a native
integer slot — it `does Int` — which is how `True` reaches a C `_Bool` or `int`
parameter. The native-call argument marshaller went straight to the numeric
catch-all instead, which has no `Bool` case, so `c_abs(True)` passed 0.

## Pins

`t/nativecall-type-surface.t` and `t/nativecall-explicitly-manage.t`. The latter
demonstrates the retained-pointer case end to end with libc's `putenv`, which
POSIX specifies as taking ownership of the caller's string — the same shape as
the documented `set_version` example, with no test-only shared library needed.
