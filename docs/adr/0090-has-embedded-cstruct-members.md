# ADR-0090: A `HAS` member is laid out by value, and its handle points into the enclosing struct

- Status: Accepted (implemented)
- Date: 2026-09-12
- Related: [#7991](https://github.com/tokuhirom/mutsu/issues/7991), ADR-0085 (ecosystem test-suite parity measurement)

## Context

NativeCall spells an **embedded** struct member — one C stores by value, not by
pointer — with the `HAS` scope declarator:

```raku
class gsl_matrix is repr('CStruct') {
    HAS gsl_vector $.vector;     # the vector's bytes, inline
    has Pointer    $.block;      # a pointer to a block
}
```

mutsu did not know the word. `HAS gsl_vector $.vector` parsed as a call to a
function named `HAS` taking `$.vector`, which died with either
`Variable $.vector used where no 'self' is available` or `Unknown function: HAS`
depending on how the parse recovered. Both are load-time failures, so **up to 44
ecosystem distributions could not `use` their own modules at all** — every
`Math::Libgsl::*`, `Raylib::Bindings`, `HarfBuzz`, `Image::Libexif`,
`Net::Ethereum`, `TCP::LowLevel`, … (`eco-cluster: 2bb876fd` / `93dd7143`, per
ADR-0085's ledger).

Parsing the word is the smaller half. `runtime::cstruct_layout` computed every
field's offset on the assumption that a CStruct-typed field is **one pointer**,
which is right for `has` and wrong for `HAS`. Accepting the declaration without
changing the layout would have put every field *after* an embedded member at the
wrong offset — and a wrong offset in this module is a silent wild read, not an
error.

## Decision

**A `HAS` member occupies its own storage inside the enclosing struct, and the
value it reads back as is a handle onto that storage.**

Concretely:

1. **`HAS` is a scope on the `has` declaration**, not a separate statement form.
   The parser accepts it wherever `has` is accepted and records one bit
   (`Stmt::HasDecl::is_embedded`) that rides the existing
   `CompiledAttrDecl` → `ClassDef::embedded_attributes` path. Every other trait,
   twigil, type-constraint and shape rule is `has`'s, unchanged.

2. **The layout carries a size and an alignment that are not properties of the
   field type.** `FieldType` gained one variant, `Embedded { size, align }`,
   whose payload is resolved by the *caller* — the only side that can compute a
   nested layout — before `layout_struct` runs. Alignment is tracked separately
   from size for the first time: an embedded `{ int32; num64 }` is 16 bytes but
   aligns to 8, so the previous `align() == size()` identity would have
   over-padded it.

3. **Reading a `HAS` member yields a handle at `base + offset`**, wrapped in the
   declared class exactly as a pointer field's target is. Reads and writes
   through it therefore land in the enclosing struct's own bytes; there is one
   struct, not a copy per access. An inline `HAS T @.x[N] is CArray` member
   reads back as a `CArray[T]` onto the same address, which is what makes
   `$s.x[2]` reach the third word of the struct.

4. **`HAS` on a type C does not hold by value lays out as an ordinary field.**
   A native scalar is already stored inline by `has`, so rakudo accepts the
   declaration, warns `Useless use of HAS scope on <type> typed attribute.`, and
   carries on; mutsu says the same thing at parse time (scalar declarations
   only — a shaped array is exactly what `HAS` is for and warns about nothing).
   A `Str`, a `Pointer[T]` or a `CArray` without a shape is one pointer, as
   before, and warns about nothing in either implementation.

5. **A layout that cannot be computed is refused, never guessed.** Two cases
   join the module's existing "one unmarshallable field aborts the whole layout"
   rule: a struct that embeds itself (no such C type exists; a thread-local
   in-progress set catches the cycle), and `HAS T @.x[N]` where `N` is a named
   constant rather than a literal, since only a literal shape survives into the
   compiled declaration. Both make `nativesizeof` fail loudly instead of
   returning a number that is wrong.

## Alternatives considered

- **Keep treating a `HAS` field as a pointer and only fix the parse.** This is
  the cheapest way to clear `blocked_load` for all 44 distributions, and it was
  rejected: it trades a loud load failure for a silent wrong answer on every
  field behind the member. A binding that loads and then reads garbage is worse
  than one that does not load.

- **Give the embedded member a copy of the bytes on read.** C semantics for
  `a.inner` are an lvalue into `a`, not a copy; a copy would make
  `$line.from.x = 1` write to a temporary and vanish.

- **Resolve a constant array shape by evaluating the attribute's default at
  layout time.** Running Raku code inside `cstruct_layout` — which is reached
  from the middle of a field read — is a re-entrancy hazard for one
  distribution's benefit. Refusing the layout is the safe answer for now; the
  right fix is to resolve non-literal shape dimensions at class-registration
  time, which is a separate change to declaration handling and not to
  NativeCall ([#8032](https://github.com/tokuhirom/mutsu/issues/8032)).

## Consequences

- `nativesizeof` agrees with rakudo on the real embedded-struct-heavy bindings
  measured for this decision: `Raylib::Bindings`' `Camera3D` (44),
  `RenderTexture2D` (44) and `Transform` (40), and `Math::Libgsl::Raw::Matrix`'s
  `gsl_vector` (40), `gsl_matrix` (48) and `gsl_block` (16).
- `FieldType` is no longer `Copy`-cheap in the sense of "the variant alone tells
  you its width". Every consumer already went through `size()`/`align()`, so
  this is contained, but a new consumer must not re-derive a width from the
  variant name.
- A pre-existing gap is now reachable from a new direction and is **not** fixed
  here: `$!private-field` on a CStruct handle resolves through the instance's
  Raku attributes rather than through native memory, so it reads as `Nil` — for
  a plain `has int32 $!a` as much as for a `HAS` member. Likewise, assigning
  into an index of a native `CArray` handle that came from a *call*
  (`$s.arr[1] = 5`) silently does nothing, because the element-assign path is
  keyed on a variable name. Both are filed separately
  ([#8030](https://github.com/tokuhirom/mutsu/issues/8030),
  [#8031](https://github.com/tokuhirom/mutsu/issues/8031)).
