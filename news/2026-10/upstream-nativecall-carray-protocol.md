# CArray views, expression-target stores and renamed mixin types

This set of fixes moves the vendored upstream NativeCall (#11203) from 59 to
63 of the 66 `is native` test files. The three still failing pin behaviour of
the native provider that Rakudo does not share.

- `nativecast(CArray[Str], $ptr)` and `nativecast(CArray[Pointer], $ptr)`
  view C memory: the array's slots are the addresses at `$ptr`. A slot reads
  as the string, or as a pointer object, at that address, and a bind writes
  the address back. Before, only a CArray of native numbers could be a view.
- An element assignment whose target is an expression, such as `get()[2] = 7`
  or `$box.c[1] = 5`, now calls the object's own `ASSIGN-POS` / `ASSIGN-KEY`,
  as the variable form already did. Before, the store went into a throwaway
  aggregate.
- A role method no longer holds a second reference to a buffer-backed
  instance's storage. The extra reference made the method's in-place element
  write copy the storage, which moved a `CArray`'s memory away from a pointer
  taken earlier.
- `.end` counts the elements of any buffer-backed instance.
- A mixin type object renamed with `.^set_name` renders by that name in
  `.raku` and `.gist`. Upstream's `Pointer[void]` is such a type.
- A defined argument that is not an array, passed for a `CArray` parameter, is
  now refused with MoarVM's representation message. Before, it failed earlier
  with an error about a missing element type.
