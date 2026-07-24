# CStruct fields are readable, and `nativecast` exists

mutsu passed an `is repr('CStruct')` value around as an **opaque native
handle** — an instance carrying the C pointer, enough to hand back to C but not
to look inside. Real bindings look inside. `OpenSSL::SSL` declares the whole
`SSL` struct and reads `$ssl.server`; `OpenSSL::CryptTools` casts an
`EVP_CIPHER*` and reads `$evp.key_len` to validate a key length. Every such
accessor answered `Nil`, because the class declares the fields as attributes but
the instance has no Raku storage for them.

`runtime::cstruct_layout` now computes each field's byte offset from the
declared attributes using the C alignment rules, and the accessor path reads the
field out of the pointed-to memory. A field declared as another native class
comes back as a handle of that class, so `$ssl.method.version`-style chains keep
working; a `Str` field is read through its `char*`. A field type NativeCall
cannot marshal aborts the whole layout rather than shifting every later field —
a wrong offset is a silent wild read, not a visible error.

NativeCall's `nativecast($target-type, $source)` is implemented alongside it:
without it there is no way to reach the fields of a struct a C function returned
as an opaque pointer.

Two supporting gaps closed on the way:

- `is repr('CPointer')` was not tracked at all. It matters here because a
  *field* of such a type (OpenSSL's `BIO`) is still one pointer wide inside the
  enclosing struct — without it, `SSL`'s layout aborted at its third field and
  no `SSL` field was readable.
- An empty (or whitespace-only) string numifies to `0` in Raku, so `"" == 0` is
  True. mutsu's numeric bridge treated it as "not a number at all" and answered
  False (`+""` was already `0`, so the two disagreed). `"" <=> 0` threw
  `X::Str::Numeric` instead of returning `Same`. Fixing this also exposed that
  `!=` did not mirror `==`: it compared operands of different shapes
  structurally, so `"" != 0` and `"" == 0` were both True/False in the wrong
  direction. `!=` now uses the same rule as `==`.

`OpenSSL 01-basic` goes 2/7 → **7/7**; the bundled-library baseline is 15/18.
Pinned by `t/nativecall-cstruct-fields.t` and
`t/empty-string-numifies-to-zero.t`, both of which also pass unchanged under
`raku`.

Writing fields, `HAS`-embedded structs/arrays, and allocating a struct from Raku
(`MyStruct.new`) remain follow-up work.
