# `Pointer[T].deref` method is missing

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/nativecall.rakudoc:598`).

## Context

NativeCall is a measured, justified rung-3 native provider with known gaps (see
`todo/deep/nativecall-cannot-be-vendored.md`). This finding is narrower than that
document's scope: the actual FFI call in the repro below (`strdup`, a plain libc function)
succeeds — the native call machinery, argument marshaling, and `Pointer[Str]` typed-pointer
construction all work. Only the `.deref` accessor method on a typed `Pointer[T]` value is
missing, which is a small, self-contained, worth-filing gap rather than part of the deep
"cannot be vendored" cluster.

## Minimal repro

```raku
use NativeCall;
sub strdup(Str $s --> Pointer[Str]) is native { * }
my Pointer[Str] $p = strdup("Success!");
say $p.deref;
```

- `raku`: `Success!`
- `mutsu`: `No such method 'deref' for invocant of type 'NativeCall::Types::Pointer[Str]'`

## Affected files (starting point)

- Wherever `NativeCall::Types::Pointer` methods are implemented — grep for
  `"Pointer"` in `src/runtime/native_methods/` / NativeCall-related modules. `.deref`
  should read the pointed-to value using the pointer's parameterized type (`Pointer[Str]`
  reads a C string; `Pointer[int32]` reads an int32; etc.), mirroring however
  `nativecast`/typed-pointer construction already resolves the parameterized type.
