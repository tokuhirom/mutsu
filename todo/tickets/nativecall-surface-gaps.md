# NativeCall surface gaps, measured against Rakudo

Inspection of everything `NativeCall.rakumod` exports, run 2026-07-29 against
Rakudo v2026.06 with `mutsu -e 'use NativeCall; …'`. This is the inventory, not
a plan — each row is small and independent, so pick them off as they block real
code rather than as a campaign.

## Subs

All five are done: each is an `our sub` in the NativeCall prelude over a
`__mutsu_`-prefixed native primitive, so it arrives with the module rather than
being ambient, and has a real `&`.

| export | mutsu | note |
| --- | --- | --- |
| `nativecast` | ✅ done 2026-07-31 | was a builtin with no `&nativecast`; see [`news/2026-07/nativecall-exports-are-module-routines.md`](../../news/2026-07/nativecall-exports-are-module-routines.md) |
| `nativesizeof` | ✅ done 2026-07-31 | same |
| `cglobal` | ✅ done 2026-07-29 | prelude sub over a native fetch |
| `explicitly-manage` | ✅ done 2026-07-31 | prelude sub returning a `NativeCall::CStr` over a deliberately-leaked buffer |
| `refresh` | ✅ done 2026-07-31 | a genuine no-op returning 1: a mutsu CStruct holds only the C address and every field access reads through it, so its fields are never stale |

`guess_library_name` and `check_routine_sanity` are `:TEST`-tagged in Rakudo and
are not part of the default surface; no reason to add them.

## Type objects

`use NativeCall` exports these as `NativeCall::Types::*`. mutsu answers a short
name, which is a cosmetic difference (`.^name`). Every type object now exists;
what remains is only that prefix:

| type | mutsu `.^name` | raku `.^name` |
| --- | --- | --- |
| `long` / `longlong` / `ulong` / `ulonglong` | `long` / … | `NativeCall::Types::long` / … |
| `size_t` / `ssize_t` | `size_t` / `ssize_t` | `NativeCall::Types::size_t` / `…::ssize_t` |
| `bool` | `bool` | `NativeCall::Types::bool` |
| `void` | `void` | `NativeCall::Types::void` |
| `CArray` / `Pointer` | `CArray` / `Pointer` | `NativeCall::Types::CArray` / `…::Pointer` |
| `OpaquePointer` | `Pointer` (an alias, so `OpaquePointer === Pointer`) | `NativeCall::Types::Pointer` (same) |

`bool`, `ssize_t` and `OpaquePointer` were **absent** until 2026-07-31 — naming
one as a term degraded to the `Str` an undeclared bareword becomes — and `void`
was declared but gated on the source *also* naming `Pointer`. Both are fixed;
see [`news/2026-07/nativecall-type-surface.md`](../../news/2026-07/nativecall-type-surface.md).

Renaming the type objects to their `NativeCall::Types::` spelling is the **one
open item left in this ticket**, and it is cosmetic: nothing in the batteries
matches on those names, and `nativecast`/`nativesizeof`/`CType::from_type_name`
all key off the short spelling. Doing it means registering the prelude classes
under the long name and teaching those lookups to strip the namespace — worth
doing for fidelity, not urgent.

## Reproducing

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

(This section absorbs PLAN.md's former "§1 B4 NativeCall remainder" bullet,
2026-08-02 — the plan file keeps the outline, the inventory lives here.)
