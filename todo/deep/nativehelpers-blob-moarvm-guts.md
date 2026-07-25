# `NativeHelpers::Blob` needs `nativesizeof` — and behind it, MoarVM's object guts

Loading anything that goes through `NativeLibs`' library-name resolution dies
with

```
Unknown function: cannon-name
```

but that is a symptom, not the cause — see "What it actually is" below. This
started life as `todo/tickets/nativelibs-our-proto-sub-unknown-function.md`; it
moved here once the real blocker turned out to need a design decision rather than
a slice.

`cannon-name` is not a typo in the caller — it is the real (mis-spelled upstream)
name of an `our proto sub` declared in `NativeLibs.rakumod`:

```raku
our proto sub cannon-name(|) {*}
multi sub cannon-name(Str:D $libname, Version $version?) { … }
multi sub cannon-name(Str $libname, Cool $ver) { … }
```

## Impact

`NativeLibs` (0.0.9, `zef:raku-community-modules`, Artistic-2.0, **96
dependents**) is a runtime dependency of *both* database candidates:

- `DB::SQLite` — this is its **first** failure; all 9 of its upstream test files
  die here (raku: 9/9 pass).
- `DBIish` — the chosen battery (`docs/batteries/database.md`) also lists
  `NativeLibs` in `depends`, so this has to be fixed for the database slot
  regardless of which candidate is bundled.

## Repro

```sh
mkdir -p tmp/dbslot && cd tmp/dbslot
curl -sSL 'https://raw.githubusercontent.com/raku/REA/main/archive/N/NativeLibs/NativeLibs%3Aver%3C0.0.9%3E%3Aauth%3Czef%3Araku-community-modules%3E.tar.gz' | tar xz
# then load DB::SQLite (or any dist whose driver calls NativeLibs::Loader)
```

## What it is NOT

A plain `our proto sub` in a module works. This parses, loads and dispatches
correctly under both implementations:

```raku
# lib/ProtoMod.rakumod
unit module ProtoMod;
our proto sub cannon-name(|) {*}
multi sub cannon-name(Str:D $n) { "one:$n" }
multi sub cannon-name(Str:D $n, Int $i) { "two:$n:$i" }
our sub use-it($n) { cannon-name($n) }
```

So the trigger is narrower than "`our proto sub` is broken". The distinguishing
feature of `NativeLibs.rakumod` is its **file shape**: it opens with a custom
`sub EXPORT(|)` that builds a `Map` by introspecting `&trait_mod:<is>.candidates`,
and only *then* declares `unit module NativeLibs`:

```raku
use NativeCall;

sub EXPORT(|) {
    my $exp = &trait_mod:<is>.candidates.first: { .signature ~~ :(Routine, :$native!) };
    Map.new('NativeCall' => NativeCall, '&trait_mod:<is>' => $exp.dispatcher);
}

unit module NativeLibs:ver<0.0.9>;

our proto sub cannon-name(|) {*}
```

That theory was **tested and is wrong** (2026-07-25). This reduction, which has
exactly that shape, works identically under both implementations:

```raku
# lib/ProtoExp.rakumod
sub EXPORT(|) { Map.new('Marker' => 42) }
unit module ProtoExp:ver<0.0.9>;
our proto sub cannon-name(|) {*}
multi sub cannon-name(Str:D $n) { "one:$n" }
multi sub cannon-name(Str:D $n, Int $i) { "two:$n:$i" }
our sub use-it($n) { cannon-name($n) }
```

`use NativeLibs;` on its own also loads fine. So the file shape is not the
trigger, and this ticket's title is a misnomer.

## What it actually is: `nativesizeof`, and MoarVM guts behind it

Bisecting the load chain shows `NativeLibs` and `DBDish` load; the failure is in
**`NativeHelpers::Blob`** (a hard `depends` of `DBIish`), and it is masked by
mutsu reporting it as a nested `An exception occurred while evaluating a CHECK`.
Unwrapped, the message is:

```
Unknown function: nativesizeof
```

`nativesizeof` is a NativeCall builtin mutsu does not implement at all
(`grep -rn nativesizeof src/` finds nothing). It is reached from
`MoarVM::Guts::REPRs`, which `NativeHelpers::Blob` uses unconditionally:

```raku
constant ptrsize is export = nativesizeof(Pointer);
```

**Implementing `nativesizeof` alone will not be enough.** The very next
statement in that file computes a constant by walking MoarVM's object header:

```raku
constant Offset = do {
    my Pointer \p = Pointer.new(0xdeadbeaf);
    my CArray[intptr] \ar = nativecast(CArray[intptr], Pointer.new(p.WHERE));
    my $i = 0;
    repeat { last if ar[$i] == p; } while ++$i < 10;
    die "Can't determine actual Offset" if $i == 10;
    $i * ptrsize;
};
```

It reads the raw memory of a *Raku* object and expects MoarVM's layout;
`BODY_OF` then casts that address to hand-written `CStruct` mirrors of MoarVM's
`VMArray` / `CArray` / `CStruct` REPR bodies. The module says as much in its own
header comment ("access the guts of MoarVM's REPRs"). Emulating that is a
different and much larger problem than adding a builtin, and it needs a design
decision before any code — treat this as a `todo/deep/` item, not a slice.

One mitigating fact: the `DBDish::SQLite` driver only uses `blob-from-pointer`
from `NativeHelpers::Blob`, which needs `nativesizeof` and a
`memcpy(Blob, Pointer, size_t)` but **not** `BODY_OF`. Only the load-time
`Offset` constant forces the guts path.

## Is it emulable? The contract, and where mutsu stands

The module does not actually require *being* MoarVM. It requires four things:

1. `nativesizeof(T)` works.
2. `obj.WHERE` is a **genuinely readable address**.
3. `nativecast(Pointer[SomeCStruct], addr).deref` reads a struct through it.
4. `obj.REPR` says `VMArray` / `CArray` / `CStruct`.

Measured against mutsu (2026-07-25, debug build):

| contract | raku | mutsu |
| --- | --- | --- |
| `nativesizeof(Pointer)` | `8` | `Unknown function: nativesizeof` |
| `Buf.new(1,2,3).REPR` | `VMArray` | `P6opaque` |
| `Buf.WHERE` | a real address | a hash of the WHICH identity — **not dereferenceable** |
| `Pointer.new(0xdeadbeaf)` | accepted | `Default constructor for 'Pointer' only takes named arguments` |
| `nativecast(CArray[uint64], p)[0]` | reads memory | silently `Nil` |

`.WHERE` is documented in `builtins/methods_0arg/dispatch_core_coerce.rs` as
deliberately identity-derived, because mutsu's scalar values are unboxed and have
no pinnable address.

### Tier 1 — make it *load* (tractable, and enough for the battery)

Give the four load-time contracts honest implementations, scoped to NativeCall
types only:

- `nativesizeof` over native scalar type objects, `Pointer`/`Str`/`CArray[T]`
  (pointer-sized) and registered CStruct classes (reuse `layout_struct`).
- `Pointer.new($addr)` positionally, as Rakudo has it.
- `Pointer.WHERE` returning the address of a small, zero-filled, memoised native
  block whose first word holds the pointer value. The probe then finds it at word
  0, so `Offset` computes as **0** — i.e. mutsu's contract is "`.WHERE` points
  straight at the payload; there is no object header". That is self-consistent
  and forward-compatible with tier 2, which can keep bodies at offset 0.
- Indexing a `CArray[T]` *native handle* (what `nativecast` returns) reads
  `address + i * sizeof(T)`, the same trust model `cstruct_layout::read_field`
  already extends to struct fields.

None of these is a fake: each is a NativeCall compatibility gap worth closing on
its own. With them, everything that does not call `BODY_OF` works — including
`blob-from-pointer`, the only entry point `DBDish::SQLite` uses.

### Tier 2 — real `BODY_OF` (blocked on the value representation)

`BODY_OF` reads a REPR body struct and `pointer-to()` pulls the **element buffer
pointer** out of it to hand to C. That is only sound if the container's elements
live in a stable native allocation.

For `CArray` and `CStruct` mutsu could almost supply this today — it already
carries a real C pointer for a nativecast handle, so `CArrayB.storage` /
`CStructB.cstruct` would be that pointer, not a copy.

For `VMArray` (Raku `Blob` / `array`) it cannot. `src/runtime/nativecall.rs`
marshals a numeric `CArray`/`Blob` by **copying into a temporary C buffer for the
duration of the call and copying back afterwards**:

```
// Copy each numeric CArray's (possibly callee-modified) C buffer back into
// the caller's Raku array, element by element, ...
```

So there is no buffer that outlives a call, and a shadow buffer handed out via
`.WHERE` would be a *copy*: a C write through `pointer-to()` would land in it and
never reach mutsu's `Vec`, silently. That is precisely the "correct only under an
incomplete analysis, therefore flaky" shape CLAUDE.md says to prefer against.

Doing tier 2 properly means backing `Blob`/`array`/`CArray` with a stable native
allocation — a **value-representation change**, which is ADR-0001 layer 3a/3b
territory. The project is willing to make that change (confirmed 2026-07-25), but
**it needs its own design and an ADR first; do not start it as a slice.** Open
questions for that design: who owns and pins the allocation across GC; how a
Raku-side `push` that reallocates is reconciled with an outstanding C pointer;
whether `.REPR` should start reporting `VMArray`/`CArray`/`CStruct`; and whether
the body structs become a documented part of mutsu's ABI or stay an internal
detail exposed only to this module.

Replacing `NativeHelpers::Blob` with a native mutsu implementation is explicitly
**not** the plan: the batteries policy (`docs/batteries/`) is to adopt community
code as-is and grow mutsu's core, with private reimplementation as a last resort.

## Note on the diagnostic

The first line mutsu prints for these files is
`Use of uninitialized value of type Any in string context`, which is a **warning
in both implementations** and not the failure. The real error is several lines
down. Do not root-cause from the first line.
