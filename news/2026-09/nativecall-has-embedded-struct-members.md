# NativeCall's `HAS` declarator: embedded struct members are laid out by value

`HAS` is how NativeCall declares a struct member C stores **by value** rather
than through a pointer:

```raku
class gsl_matrix is repr('CStruct') {
    HAS gsl_vector $.vector;
    has Pointer    $.block;
}
```

mutsu did not know the word, so the line parsed as a call to a function named
`HAS` taking `$.vector` and died with `Variable $.vector used where no 'self' is
available` — or, when the parse recovered the other way, `Unknown function:
HAS`. Both are load-time failures, which is why the ecosystem ledger
([#7991](https://github.com/tokuhirom/mutsu/issues/7991), clusters `2bb876fd`
and `93dd7143`) counted **up to 44 distributions that could not `use` their own
modules at all**: every `Math::Libgsl::*`, `Raylib::Bindings`, `HarfBuzz`,
`Image::Libexif`, `Net::Ethereum`, `TCP::LowLevel`, `Node::Ethereum::KZG`, …

## What landed

`HAS` is now a scope on the ordinary `has` declaration — same traits, same
twigils, same type constraints — carrying one bit through to the class registry.
The interesting half is the layout. `runtime::cstruct_layout` used to give every
CStruct-typed field one pointer's width, which is right for `has` and wrong for
`HAS`; accepting the declaration without fixing that would have put every field
*behind* an embedded member at the wrong offset, and a wrong offset there is a
silent wild read rather than an error.

So `FieldType` grew an `Embedded { size, align }` variant whose payload is
resolved from the member class's own layout before the offsets are computed.
Alignment is tracked apart from size for the first time — an embedded
`{ int32; num64 }` is 16 bytes but aligns to 8, and the old `align() == size()`
identity would have over-padded it. Reading a `HAS` member yields a handle at
`base + offset`, so reads and writes through it land in the enclosing struct's
own bytes: there is one struct, not a copy per access.

`HAS T @.x[N] is CArray` — the inline *array* form, which `kazmath`'s `kmMat4`
and `Image::Libexif`'s `ExifData` both use — occupies all N elements and reads
back as a `CArray[T]` onto the same storage, so `$s.x[2]` reaches the third word
of the struct. Fixing that turned up an adjacent bug and fixed it too: an
ordinary `has CArray[T] $.x` field used to read back as a bare `Pointer`, which
cannot be indexed at all.

`HAS` on a type C does not hold by value inlines nothing, since a plain `has`
already stores a native scalar that way. Rakudo accepts the declaration, warns
`Useless use of HAS scope on int32 typed attribute.` and carries on; mutsu now
says the same thing, for scalar declarations only — a shaped array is exactly
what `HAS` is for and warns about nothing in either implementation.

Two layouts are refused rather than guessed, joining the module's existing "one
unmarshallable field aborts the whole layout" rule: a struct that embeds itself
(no such C type exists), and `HAS T @.x[N]` where `N` is a named constant rather
than a literal, since only a literal shape survives into the compiled
declaration. `nativesizeof` fails loudly there instead of returning a wrong
number ([#8032](https://github.com/tokuhirom/mutsu/issues/8032) tracks resolving
a constant dimension properly).

## Measured against the real bindings

`nativesizeof` agrees with rakudo on every embedded-struct-heavy binding checked:

| | rakudo | mutsu |
|---|---|---|
| `Raylib::Bindings` `Camera3D` / `RenderTexture2D` / `Transform` | 44 / 44 / 40 | 44 / 44 / 40 |
| `Math::Libgsl::Raw::Matrix` `gsl_vector` / `gsl_matrix` / `gsl_block` | 40 / 48 / 16 | 40 / 48 / 16 |

`Math::Libgsl::Raw::Matrix` (the issue's headline site), `Image::Libexif`,
`Image::Libexif::Raw`, `Raylib::Bindings` and `Net::Ethereum::Utils` all load
now; what remains failing in that corpus is missing *dependencies*, not `HAS`.

The decision is recorded as
[ADR-0090](../../docs/adr/0090-has-embedded-cstruct-members.md); the regression
test is `t/nativecall/nativecall-has-embedded-struct.t`, which pins the layout,
the inline-array form, and the warning, and passes under rakudo too.

## Known adjacent gaps, filed separately

Reading `$!private-field` on a CStruct handle still resolves through the
instance's Raku attributes rather than native memory (so it comes back `Nil`) —
for a plain `has int32 $!a` as much as for a `HAS` member. And assigning into an
index of a native `CArray` handle that came from a *call* (`$s.arr[1] = 5`)
silently does nothing, because the element-assign path is keyed on a variable
name. Neither is new, and neither is caused by this change —
[#8030](https://github.com/tokuhirom/mutsu/issues/8030) and
[#8031](https://github.com/tokuhirom/mutsu/issues/8031).
