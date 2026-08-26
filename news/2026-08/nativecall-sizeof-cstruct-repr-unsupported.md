# `nativesizeof` on a CStruct with a bare `CArray` field

`docs/batteries/compression.md` reported that `Compress::Zlib::Raw`'s
`nativesizeof(z_stream)` failed under mutsu with

```
NativeCall op sizeof expected type with CPointer, CStruct, CArray, P6int or
P6num representation, but got a P6opaque (Compress::Zlib::Raw::z_stream)
```

The message names `CStruct` among the things it expects and then calls the class
a `P6opaque`, which reads as "the `is repr('CStruct')` trait was not recorded".
That is not what happened.

## Verified against the real dist, not a reconstruction

A later survey pass marked this ticket "possibly already fixed" because a
hand-written `class z_stream is repr('CStruct')` gave the same answer as `raku`.
It was not fixed — the reconstruction had simply guessed the field list. The
dist itself (`Compress::Zlib::Raw` from `P6-Compress-Zlib-Raw`) still failed, and
the field that mattered was one nobody would think to invent.

## Root cause

`is_cstruct_class` recognised the repr fine. `native_size_of_type` then asked for
the struct's **layout**, and `layout_struct` deliberately aborts the whole layout
when it meets a field type it cannot marshal — continuing would give every later
field a wrong offset, and a wrong offset is a silent wild read. `None` from the
layout was then reported with the P6opaque message, which described neither the
cause nor the offending field.

The unmarshallable field is `has CArray $.next-in` — a **bare, unparameterised**
`CArray`. `FieldType::from_type_name` accepted `CArray[T]` (via a
`starts_with("CArray[")` prefix test) and any `Pointer[T]`, but had no arm for
`CArray` on its own, even though it is one pointer in C exactly like the
parameterised spelling. So one field aborted the layout of a thirteen-field
struct.

`CArray` now sits alongside `Pointer` / `OpaquePointer` in the pointer arm.

## Measured result

`nativesizeof(z_stream)` returns **112** under both mutsu and raku — matching the
comment the dist itself carries at the call site (`# 112 == sizeof z_stream (64
bit linux)`), and `nativesizeof(gz_header)` returns 80. `Compress::Zlib::Raw`'s
own `t/01-basic.t` passes 7/7 under mutsu, the same as raku.

The higher-level `Compress::Zlib` wrapper is still blocked, but by a different,
already-filed bug —
[`nativecall-local-sub-shadows-imported-same-name.md`](../../todo/tickets/nativecall-local-sub-shadows-imported-same-name.md)
— exactly as that ticket predicted ("fixing the shadowing bug alone will not get
`t/02-stream.t`/`t/03-wrap.t` passing; this CStruct-sizeof gap also needs to be
closed"). Now only the shadowing bug remains.

## Pin

`t/nativecall-pointer-and-cglobal.t` compares `nativesizeof` of a struct whose
first field is a bare `CArray` against the same struct written with `Pointer`,
which is portable across LP64 platforms rather than hard-coding a byte count.
