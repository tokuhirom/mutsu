# `Pointer[T].deref` works on a native sub's return value — and no longer segfaults

Found by the doc-diff harness (`Language/nativecall.rakudoc:598`), which reported
that the documented `strdup` example died with "No such method 'deref' for
invocant of type 'NativeCall::Types::Pointer[Str]'":

```raku
use NativeCall;
sub strdup(Str $s --> Pointer[Str]) is native { * }
my Pointer[Str] $p = strdup("Success!");
say $p.deref;            # raku: Success!
```

## Two root causes, not one

**1. A `--> Pointer[T]` return was not a typed pointer at all.**
`CType::from_type_name` recognises only the bare `Pointer` spelling, so
`Pointer[Str]` fell through to the "user-declared native handle class" arm of the
return-type resolution in `vm_register_sub_ops.rs`. That set `ret_struct =
Some("Pointer[Str]")` and the marshaller built an instance of a *class literally
named* `Pointer[Str]`. ADR-0056 keeps a typed pointer's parameterisation in an
`of` attribute on an ordinary `Pointer` instance precisely so that every
`Pointer` method keeps working; an instance tagged `Pointer[Str]` matched
`try_pointer_method`'s `rsplit("::") == "Pointer"` guard against nothing, so
neither `.of` nor `.deref` resolved. The return path now recognises a
parameterised `Pointer[T]` and builds `make_typed_pointer(addr, T)`, the same
object `nativecast(Pointer[T], …)` produces. The two construction paths had
silently diverged; `t/nativecall-pointer-and-cglobal.t` now pins that they agree.

**2. `.deref` on a `Pointer[Str]` crashed the process.** `.deref` was implemented
as "element 0 of the equivalent `CArray[T]`", which is right for a numeric `T`
and wrong for `Str`: a `CArray[Str]` element is a `char*` *stored at* the
address, so `Pointer[Str].deref` loaded eight bytes of the string itself and
dereferenced them as a pointer. `strdup("Success!").deref` segfaulted.

Rakudo defines `Pointer.deref` as `nativecast(self.of, self)`, and mutsu now does
too. The shared core lives in the new `src/runtime/nativecall_cast.rs`, which
also took over `try_pointer_method` from the oversized `cstruct_layout.rs`. The
rule the two spellings now share is the C one:

| target | result |
| --- | --- |
| `Str` | the NUL-terminated string **at** the address |
| `int32` / `num64` / … | the scalar **at** the address |
| `Pointer` / `OpaquePointer` | a `Pointer` holding the **same** address |
| `Pointer[T]` | ditto, remembering `T` in an `of` attribute |
| `CArray` / a CStruct / CPointer / CUnion class | a handle on the same address |

Unifying them also fixed `nativecast(int32, $ptr)`, which used to build a
nonsense opaque handle tagged `int32` instead of reading the value through.

## Pin

`t/nativecall-pointer-and-cglobal.t` (`.of`, `.deref`, `.^name`, and the
construction-path agreement, using only libc symbols present on Linux and macOS).
