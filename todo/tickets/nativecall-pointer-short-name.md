# mutsu's NativeCall `Pointer` is named `Pointer`, raku's is `NativeCall::Types::Pointer`

(Merged 2026-08-10: absorbs `nativecall-surface-gaps.md`, which tracked the
exact same open item — every other row in that ticket's inventory was
already done. Its `Reproducing` probe script and the "Bigger, tracked
separately" section — by-value CStructs/callbacks, a returned `CArray[T]`
surfacing as a raw `Pointer`, and the ADR-0015 P3b/P3c native-backed-array
items — are preserved below.)

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

## Full type-object inventory (from the merged ticket)

`use NativeCall` exports these as `NativeCall::Types::*`. Every type object
exists in mutsu; only the `.^name` prefix differs:

| type | mutsu `.^name` | raku `.^name` |
| --- | --- | --- |
| `long` / `longlong` / `ulong` / `ulonglong` | `long` / … | `NativeCall::Types::long` / … |
| `size_t` / `ssize_t` | `size_t` / `ssize_t` | `NativeCall::Types::size_t` / `…::ssize_t` |
| `bool` | `bool` | `NativeCall::Types::bool` |
| `void` | `void` | `NativeCall::Types::void` |
| `CArray` / `Pointer` | `CArray` / `Pointer` | `NativeCall::Types::CArray` / `…::Pointer` |
| `OpaquePointer` | `Pointer` (an alias, so `OpaquePointer === Pointer`) | `NativeCall::Types::Pointer` (same) |

All five exported subs (`nativecast`, `nativesizeof`, `cglobal`,
`explicitly-manage`, `refresh`) are done — each is an `our sub` in the
NativeCall prelude over a `__mutsu_`-prefixed native primitive. See
[`news/2026-07/nativecall-exports-are-module-routines.md`](../../news/2026-07/nativecall-exports-are-module-routines.md)
and [`news/2026-07/nativecall-type-surface.md`](../../news/2026-07/nativecall-type-surface.md).
`guess_library_name` and `check_routine_sanity` are `:TEST`-tagged in Rakudo
and are not part of the default surface; no reason to add them.

### Reproducing

```sh
for t in long longlong ulong ulonglong bool size_t ssize_t void CArray Pointer OpaquePointer; do
  printf '%-14s mutsu=' "$t"
  mutsu -e "use NativeCall; say $t.^name" 2>&1 | head -1
done
for f in explicitly-manage refresh nativesizeof nativecast cglobal; do
  printf '%-20s ' "$f"
  mutsu -e "use NativeCall; say defined(&$f) ?? 'present' !! 'MISSING'" 2>&1 | head -1
done
```

## Bigger, tracked separately

- ~~**`is native` on methods**~~ — **done 2026-07-29**, see
  [`news/2026-07/nativecall-cglobal-and-native-methods.md`](../../news/2026-07/nativecall-cglobal-and-native-methods.md).
- ~~**`nativecast` tags handles with the short class name**~~ — **done
  2026-07-29**, see
  [`news/2026-07/cstruct-handles-carry-their-registered-name.md`](../../news/2026-07/cstruct-handles-carry-their-registered-name.md).
- **By-value CStructs and callbacks** — recorded in `runtime/nativecall.rs`'s
  module docs as follow-up work; no dist in the batteries needs them yet.
  (Argument-position CStructs work: a struct is an opaque native handle passed
  by pointer, which is what made the genuine OpenSSL + IO::Socket::SSL binding
  run a real `https://` GET — see
  [`news/2026-07/openssl-battery-https.md`](../../news/2026-07/openssl-battery-https.md).
  What is missing is **by-value** field-layout marshalling and generic C
  callbacks.)
- **A *returned* `CArray[T]` is surfaced as the raw `Pointer` it carries** —
  there is no length with which to reify a Raku array, so the return is handed
  back as the pointer instead of an indexable `CArray`. Rakudo returns a
  `CArray` you can index (reading past the end is the caller's problem).
- **Native-backed `array[T]` (ADR-0015 P3b) and reference-element
  `CArray[Str]`/`CArray[Pointer]` (P3c)** are tracked in
  [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md),
  not here. P3b also fixes `array-shapes.t` T36-38.
