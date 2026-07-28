# mutsu's NativeCall `Pointer` is named `Pointer`, raku's is `NativeCall::Types::Pointer`

```
$ raku  -e 'use NativeCall; say Pointer.^name; say Pointer[uint8].^name'
NativeCall::Types::Pointer
NativeCall::Types::Pointer[uint8]

$ mutsu -e 'use NativeCall; say Pointer.^name; say Pointer[uint8].^name'
Pointer
Pointer[uint8]
```

mutsu's `Pointer` comes from the builtin prelude in `runtime/run.rs`
(`NATIVECALL_POINTER_PRELUDE`), which declares it as `GLOBAL::Pointer` — i.e.
under the short name in the global namespace. Raku's lives in the
`NativeCall::Types` package and is imported into the user's scope by
`use NativeCall`, so its `.^name` carries the full package path while the bare
name still resolves.

Nothing in the batteries has tripped on this yet; it surfaces as a cosmetic
difference in `.^name`, `.gist` of the *type object*, and error messages. (The
`.gist` of an *instance* already hard-codes the raku spelling —
`NativeCall::Types::Pointer<NULL>` — so the two disagree with each other today.)

## Why it is not a one-liner

Renaming the prelude class to `NativeCall::Types::Pointer` means the bare name
`Pointer` has to keep resolving, which is an import alias, not a rename. The
short name is also matched *by name* in several places that would all have to
learn the qualified spelling — at least:

- `runtime_class_query::is_non_parametric_type` (the `"Pointer"` allow-list entry
  that makes `Pointer[T]` legal at all),
- `cstruct_layout` (already half-aware: it accepts a qualified
  `NativeCall::Types::Pointer[T]` base when parsing a field type),
- the marshalling layer's pointer-argument and return-value recognition.

Worth doing as one deliberate slice — "give the NativeCall prelude its real
package and import the short names" — rather than piecemeal. Doing it piecemeal
risks a name-exact guard falling through, which is the failure mode ADR-0015
§2.1 warns about.
