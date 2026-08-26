# A `&callback (...)` NativeCall parameter is not marshalled to a C function pointer

Found 2026-08-26 measuring `Archive::Libarchive::Raw` v0.1.5 under mutsu after
the CPointer/library-spec fixes took it from 1/6 to 5/6 files (see
[`news/2026-08/nativecall-cpointer-repr-typed-param-returns-whatever.md`](../../news/2026-08/nativecall-cpointer-repr-typed-param-returns-whatever.md)).
This is the remaining blocker for the sixth file.

## Repro

`lib/Archive/Libarchive/Raw.rakumod` declares three callback parameters with the
documented `&name (Sig)` spelling (`Language/nativecall.rakudoc`'s "Function
arguments" section):

```raku
sub archive_write_open(archive, int64,
                       &open  (archive, int64 --> int32),
                       &write (archive, int64, CArray[uint8], size_t --> size_t),
                       &close (archive, int64 --> int32) --> int32)
    is native(LIB) is export { * }
```

and `t/05-archive-read-disk.rakutest` calls it with three Raku subs:

```raku
is archive_write_open($w, 42, &archive-open, &archive-write, &archive-close),
   ARCHIVE_OK, 'write open';
```

- `raku`: passes (the whole file is 56/56).
- `mutsu`:
  ```
  X::TypeCheck::Argument: Type check failed for __type_only__: expected archive, got Sub
    in block <unit> at t/05-archive-read-disk.rakutest line 96
  ```

The `__type_only__` in the message says the parameter was recorded as an
anonymous *type* constraint, so the `&name (Sig)` form is being read as an
ordinary typed parameter rather than as a callback, and the declared callback
signature is then matched against the passed `Sub`.

## Relationship to the existing parser ticket

[`libzip-nativecall-callback-signature-type-parse.md`](libzip-nativecall-callback-signature-type-parse.md)
records the *anonymous* spelling `& (Pointer, Pointer, int64, int32 --> int64)`
failing to parse at all. This one is the *named* spelling, which parses but is
not marshalled. They are probably two ends of the same missing feature and are
best done together; keep both files until one fix closes both repros.

## What the fix needs

Two halves, and the second is the substantial one:

1. Record a `&name (Sig)` / `& (Sig)` parameter as a callback rather than a type
   constraint (`src/parser/` signature parsing, then `ParamSpec` in
   `src/runtime/nativecall.rs` needs a `CType` for "function pointer").
2. Build a real C function pointer that re-enters the VM. libffi's *closure* API
   is the mechanism (`libffi::middle::Closure`), and the hard parts are the ones
   every FFI callback implementation has: the closure must outlive the C call
   that stores it (libarchive keeps all three for the lifetime of the archive
   handle, so a closure dropped at the end of `archive_write_open` would leave
   libarchive calling freed memory), and the callback body runs on whatever
   thread C chose, so it has to reach an `Interpreter` safely.

mutsu already has the *reverse* direction — `nativecast(:(...), $ptr)` turns a C
function pointer into something callable, via `NativeCallSpec::entry` in
`src/runtime/nativecall_fnptr.rs`. That is the natural place to grow the
outbound half.

## Priority

Medium. It is the last blocker for `Archive::Libarchive::Raw`'s own suite (5/6
files pass now) and for the higher-level `Archive::Libarchive`, which
`docs/batteries/compression.md` rates the strongest archive-battery candidate.
But it is genuinely deeper than the fixes that got the dist to 5/6 — closure
lifetime and re-entrancy are design questions, not a missing match arm — so it
should be scoped on its own rather than tacked onto a NativeCall bug-fix PR. If
the design ends up needing an ADR, move this to `todo/deep/`.
