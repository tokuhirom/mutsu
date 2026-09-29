# Buf/Blob multi-dispatch narrowness comes from the type catalog

Multi-dispatch ranked every builtin type by the builtin type catalog except the
Buf/Blob family, which kept a private table keyed by the `buf8`/`blob8`
spellings. That table disagreed with Rakudo. It said a `utf8` (what `"a".encode`
returns) is not a `blob8`, so `multi f(blob8 $)` lost to `multi f(Blob $)` for
it, while raku picks `blob8`.

The catalog's narrowness chain now keeps a parameterized name next to its base,
for example `Buf[uint8]` then `Buf`, and `Blob[uint8]` then `Blob`, so a sized
spelling ranks narrower than the bare role. Dispatch maps the `bufN`/`blobN`
source spellings onto the catalog names, and the private table is gone (#10133).
