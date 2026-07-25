# `nativesizeof`, a real `Pointer.WHERE`, and reads through a `CArray` handle

`NativeHelpers::Blob` — a hard dependency of the `DBIish` battery — could not be
loaded at all. mutsu reported it as a nested `An exception occurred while
evaluating a CHECK`; unwrapped, the first cause was `Unknown function:
nativesizeof`.

Behind that sat `MoarVM::Guts::REPRs`, which the module uses unconditionally and
which does something unusual: it derives, at load time, how far into an object
that object's payload lives, by building a `Pointer` around a known sentinel and
scanning `.WHERE` for it.

```raku
constant ptrsize is export = nativesizeof(Pointer);

constant Offset = do {
    my Pointer \p = Pointer.new(0xdeadbeaf);
    my CArray[intptr] \ar = nativecast(CArray[intptr], Pointer.new(p.WHERE));
    my $i = 0;
    repeat { last if ar[$i] == p; } while ++$i < 10;
    die "Can't determine actual Offset" if $i == 10;
    $i * ptrsize;
};
```

Measured against mutsu, every step of that failed for a different reason. Each
turned out to be an ordinary NativeCall compatibility gap worth closing on its
own:

| what the module needs | mutsu before |
| --- | --- |
| `nativesizeof(Pointer)` | `Unknown function: nativesizeof` |
| `Pointer.new(0xdeadbeaf)` | `Default constructor for 'Pointer' only takes named arguments` |
| `p.WHERE` | a hash of the object's `WHICH` identity — not dereferenceable |
| `nativecast(CArray[T], p)[0]` | silently `Nil` |
| `my CArray[intptr] \ar = …` | type check failed against the resolved `CArray[uint64]` |

## What changed

- **`nativesizeof`** reports the width of a native scalar, one pointer for
  anything C holds by reference, and the *padded total size* of a
  `is repr('CStruct')` class (reusing the existing `layout_struct`). Verified
  field-for-field against raku, including the padding cases —
  `CStruct { int8, int32, int8 }` is 12, `CStruct { Pointer, int32 }` is 16.
- **`Pointer.new($address)`** takes the address positionally, as Rakudo does.
  The named `:address` form Rakudo ignores is kept, because mutsu accepted it
  before it had the positional one.
- **`Pointer.WHERE` is a real, readable address.** mutsu's values are unboxed and
  have no pinnable address, so `.WHERE` is normally derived from object identity
  — fine until a binding *dereferences* it. A `Pointer` now gets a small,
  zero-filled, memoised native block whose first word holds the pointer value, so
  the scan above terminates. The contract this establishes is: **mutsu's
  `.WHERE` points straight at the payload, with no object header in front of
  it**, i.e. the offset the probe computes is 0.
- **Indexing a `CArray[T]` *native handle*** — what `nativecast(CArray[T], $ptr)`
  returns, a bare C address with no Raku-side storage — reads element `i` out of
  native memory, the same trust `cstruct_layout::read_field` already extends to
  struct fields. (Reading past the end is undefined behaviour here exactly as it
  is in Rakudo, which segfaults on the same input.)
- **A `CArray[T]` handle satisfies a `CArray[U]` constraint** when the element
  types agree, and a type argument written as a `constant` type alias
  (`constant intptr = uint64; my CArray[intptr] $x`) resolves before the
  comparison — the value already carries the resolved spelling, so only the
  declared side needed it.

One of these was a trap worth recording. The prelude's `Pointer` class picks up
the enclosing package when it is prepended inside a module, so it is
`Probe::Pointer` there, not `Pointer`. A `.WHERE` special case keyed on the exact
name therefore missed it inside every module — and falling through to the
identity hash handed a binding a garbage address to dereference, which
**segfaulted**. The class is now matched on its last `::` component, the same
"one class, several spellings" problem `cstruct_class_name` already documents.

## Impact

`NativeHelpers::Blob` and `MoarVM::Guts::REPRs` now load. The four `DBIish` files
that were dying inside a `CHECK` get past it and reach the module's real
remaining blocker (`Unknown function: cannon-name`).

This is deliberately the *load-time* half of the problem. `BODY_OF` and
`pointer-to()`, which hand C the address of a container's element buffer, need
that buffer to be a stable native allocation — mutsu copies into a temporary one
per call — and that is a value-representation change with its own design work.
It is written up in `todo/deep/nativehelpers-blob-moarvm-guts.md`.

Pinned by `t/nativecall-sizeof-pointer-where.t`, which passes identically under
`raku`.
