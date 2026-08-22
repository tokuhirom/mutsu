# NativeCall `nativesizeof` does not support `repr('CStruct')` classes

## Repro

`Compress::Zlib::Raw.pm6` (REA `Compress::Zlib::Raw` v1.0.1) declares a
CStruct-repr class for zlib's `z_stream`:

```raku
class z_stream is repr('CStruct') is export {
    has ...;
}
```

and elsewhere computes its size for a native call:

```raku
sub z_stream_sizeof(...) { nativesizeof(z_stream) }
```

Under `raku` this works. Under mutsu:

```
$ mutsu -I lib t/02-stream.t
NativeCall op sizeof expected type with CPointer, CStruct, CArray, P6int or
P6num representation, but got a P6opaque (Compress::Zlib::Raw::z_stream)
  in sub nativesizeof at lib/Compress/Zlib/Raw.pm6 line 2
  in sub z_stream_sizeof at lib/Compress/Zlib/Raw.pm6 line 56
```

The error message names `repr('CStruct')` as one of the types it expects, but
then reports the actual class as `P6opaque` — i.e. mutsu is not recognizing
the class's own `is repr('CStruct')` trait when `nativesizeof` looks up its
representation.

## Where found

`docs/batteries/compression.md` survey (2026-08-22), `Compress::Zlib`'s own
streaming API (`t/02-stream.t`, `t/03-wrap.t`) — both fail this way.

## Affected files

NativeCall / repr handling, likely `src/runtime/` NativeCall support code and
wherever `repr('CStruct')` traits are recorded on a class (search for
`CStruct` and `nativesizeof`).

## Notes

This is a second, independent blocker for `Compress::Zlib` beyond
[nativecall-local-sub-shadows-imported-same-name.md](nativecall-local-sub-shadows-imported-same-name.md) —
fixing the shadowing bug alone will not get `t/02-stream.t`/`t/03-wrap.t`
passing; this CStruct-sizeof gap also needs to be closed.
