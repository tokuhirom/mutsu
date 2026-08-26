# `Pointer[T]`'s `.raku` / `.gist` / `.^name` match Rakudo

Found by the doc-diff harness (`Language/nativetypes.rakudoc:172`):

```raku
use NativeCall;
sub malloc(int32 $size --> Pointer[void]) is native { * }
say malloc(32).raku;
```

- raku: `NativeCall::Types::Pointer[NativeCall::Types::void].new(297902560)`
- mutsu: `NativeCall::Types::Pointer[void].new(address => 128219605755824)`

Two deterministic differences, both now closed: the type parameter rendered as
the bare `void` instead of the fully-qualified `NativeCall::Types::void`, and the
constructor call rendered as a named argument instead of Rakudo's positional
form. (The address itself is allocator-dependent and was never the bug.)

## Root cause

Everything human-facing about a `Pointer` was written out longhand in three
different places instead of being derived from one name.

`qualify_nativecall_type_name` (`src/value/display.rs`, ADR-0056's display-only
qualification) preserved a `[T]` suffix verbatim rather than qualifying `T` the
same way it qualifies the head — so `void`, itself one of the NativeCall builtin
type names, stayed bare. It now recurses through `user_facing_type_name`, which
also leaves a non-NativeCall parameter alone (`Pointer[Str]`, as Rakudo does).

The `.raku` rendering came from the generic instance-repr path
(`Class.new(attr => value)`), which cannot know that `Pointer.new` takes a
positional. The prelude class in `src/runtime/run.rs` now declares its own
`method raku`, and both it and the existing `method gist` are written in terms of
`self.^name` rather than a hard-coded `'NativeCall::Types::Pointer'` literal. So
all three renderings follow one source of truth.

That source is `.^name`, which had to learn the parameterisation: ADR-0056 keeps
`T` in an `of` attribute rather than in the class name (so that every `Pointer`
method and the marshalling layer's `address` read keep working), so
`dispatch_caret_name`'s instance arm re-attaches it via the new
`nativecall::pointer_display_suffix`. Both methods also handle an undefined
invocant now — `Pointer.raku` on the bare type object used to have no `$!address`
to read.

## Pin

`t/nativecall-pointer-and-cglobal.t` — `.raku`, `.gist` and `.^name` for a typed
pointer, an untyped one, a NULL one, and the type objects `Pointer`,
`Pointer[void]` and `Pointer[Str]`. The whole file passes unchanged under real
`raku`.

## Not changed

`Pointer.Str` still returns the gist. Rakudo inherits `Mu.Str` there, which
renders `NativeCall::Types::Pointer[…]<objectid>` — an object id mutsu has no
equivalent of, and nothing useful to copy.
