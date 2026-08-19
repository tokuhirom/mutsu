# Bare `array` type constraint now matches native arrays; `NativeHelpers::Blob`'s `pointer-to(array:D)` works

ADR-0015's P3b acceptance criterion had two halves: native-backed `array[T]`
storage (landed earlier) and `pointer-to(array:D)` actually dispatching to the
right `NativeHelpers::Blob` `multi` candidate. The second half was blocked by a
dispatch bug, not storage: mutsu's *bare* `array` type constraint (as opposed
to the parameterized form `array[uint8]`) matched no value at all.

```raku
my array[uint8] $a .= new(1,2);
sub p(array[uint8] \x) { "param" }
sub b(array \x)        { "bare" }
say p($a);      # worked: param
say b($a);      # died — "bare array" never matched, even though it should
```

Root cause: `src/runtime/types/type_matching.rs` special-cased native
containers only after `parse_generic_constraint` split a `base[inner]` form,
so `array[uint8]` matched via the parameterized arm's
`container_type_metadata()` lookup, but the bare `array` spelling fell through
to a plain `value_type_name` string compare, which reports `"Array"` for a
`Value::Array` — never `"array"`. The bare-`CArray` case already had this
special-casing; bare `array` did not.

The fix mirrors the existing bare-`CArray` arm: when the constraint is exactly
`array` and the value is a native array, read its `container_type_metadata()`
`declared_type` and match when it is `array` or starts with `array[`. This is
purely additive — since bare `array` previously matched nothing, this can only
turn a `False` into a correct `True`. A plain (non-native) `Array` still
correctly answers `False` for a bare `array` constraint, matching raku (`array`
and `Array` are distinct types).

This makes `NativeHelpers::Blob`'s `multi sub pointer-to(array:D \arr, :$typed)`
selectable, closing ADR-0015 P3b in full. Pinned by `t/bare-array-type-match.t`,
which also exercises the real `pointer-to($native_array)` call end-to-end
against the bundled battery.

Two small, unrelated parity gaps were found alongside this fix and are
tracked separately in `todo/deep/nativehelpers-blob-moarvm-guts.md` (kept at
that path because several other documents link to it): a `:D`/`:U` smiley on
a lowercase native type name in term position (`array:D`, `int:D`) fails to
parse, and a native `array[T]` still smartmatches `Array` as `True` when raku
says `False` (deferred — no bundled consumer depends on the distinction yet).
