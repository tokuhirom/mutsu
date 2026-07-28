# NativeCall surface gaps, measured against Rakudo

Inspection of everything `NativeCall.rakumod` exports, run 2026-07-29 against
Rakudo v2026.06 with `mutsu -e 'use NativeCall; …'`. This is the inventory, not
a plan — each row is small and independent, so pick them off as they block real
code rather than as a campaign.

## Subs

| export | mutsu | note |
| --- | --- | --- |
| `nativecast` | works, but as a **builtin** and with no `&nativecast` | [`nativecall-exports-are-not-builtins.md`](nativecall-exports-are-not-builtins.md) |
| `nativesizeof` | same | same |
| `cglobal` | ✅ done 2026-07-29 | prelude sub over a native fetch |
| `explicitly-manage` | **missing** | `explicitly-manage($str)` — hands a `Str`'s buffer's lifetime to the callee. Documented in `nativecall.rakudoc` §"Explicit memory management" |
| `refresh` | **missing** | `refresh($obj)` — re-read a CStruct's fields after C wrote them behind mutsu's back. Under ADR-0015's shared-storage direction this may become a no-op rather than a copy; decide when implementing |

`guess_library_name` and `check_routine_sanity` are `:TEST`-tagged in Rakudo and
are not part of the default surface; no reason to add them.

## Type objects

`use NativeCall` exports these as `NativeCall::Types::*`. mutsu answers a short
name, which is a cosmetic difference (`.^name`), except where the type object
does not exist at all:

| type | mutsu `.^name` | raku `.^name` |
| --- | --- | --- |
| `long` / `longlong` / `ulong` / `ulonglong` | `long` / … | `NativeCall::Types::long` / … |
| `size_t` | `size_t` | `NativeCall::Types::size_t` |
| `void` | `void` | `NativeCall::Types::void` |
| `CArray` / `Pointer` | `CArray` / `Pointer` | `NativeCall::Types::CArray` / `…::Pointer` |
| **`bool`** | `Str` — **absent** | `NativeCall::Types::bool` |
| **`ssize_t`** | `Str` — **absent** | `NativeCall::Types::ssize_t` |
| **`OpaquePointer`** | `Str` — **absent** | `NativeCall::Types::Pointer` (an alias) |

`Str` here is what an undeclared bareword degrades to, so those three are simply
not declared. Note `OpaquePointer` *is* accepted in a **signature**
(`CType::from_type_name` maps it), so only its use as a term is missing — which
is what `my $p = OpaquePointer;` and `$x ~~ OpaquePointer` need.

Also note the short-name answers are not merely cosmetic for `void`: the
`Pointer`/`void` prelude classes are injected **only when the source contains
"Pointer"** (`inject_nativecall_prelude`), so `use NativeCall; say void.^name`
alone does not see `void`. Widening that gate is part of the same cleanup as
[`nativecall-exports-are-not-builtins.md`](nativecall-exports-are-not-builtins.md).

## Reproducing

```sh
for t in long longlong ulong ulonglong bool size_t ssize_t void CArray Pointer OpaquePointer; do
  printf '%-14s mutsu=' "$t"
  mutsu -e "use NativeCall; my \$p = Pointer; say $t.^name" 2>&1 | head -1
done
for f in explicitly-manage refresh nativesizeof nativecast cglobal; do
  printf '%-20s ' "$f"
  mutsu -e "use NativeCall; say defined(&$f) ?? 'present' !! 'MISSING'" 2>&1 | head -1
done
```

## Bigger, tracked separately

- ~~**`is native` on methods**~~ — **done 2026-07-29**, see
  [`news/2026-07/nativecall-cglobal-and-native-methods.md`](../../news/2026-07/nativecall-cglobal-and-native-methods.md).
- **`nativecast` tags handles with the short class name**, so a CStruct declared
  inside a module loses its hand-written methods:
  [`nativecast-tags-handles-with-the-short-class-name.md`](nativecast-tags-handles-with-the-short-class-name.md).
- **By-value CStructs and callbacks** — recorded in `runtime/nativecall.rs`'s
  module docs as follow-up work; no dist in the batteries needs them yet.
