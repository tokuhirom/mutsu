# A CStruct field write reaches C memory, and `.^array_type` answers

Assigning to a field through an `is repr('CStruct')` handle reported success and
did nothing:

```raku
my $s = nativecast(Pair2, calloc(1, 16));
$s.a = 42;
say $s.a;        # raku: 42    mutsu (before): 0
```

A CStruct handle keeps no Raku attributes — the C struct its `address` points at
is the only storage it has — so the assignment fell through to the ordinary
attribute path and landed in a map nothing reads. `cstruct_layout.rs` had
`read_field` and no write half, so half of the accessor pair simply did not
exist. It does now, for every field type the reader handles: the integer and
float widths, an address (however it is spelled — a `Pointer`, another CStruct
handle, or a bare `Int`), and `Str`.

A `Str` field stores a `char*`, so the bytes have to outlive the assignment: C
reads them whenever it likes. Rakudo keeps the Raku `Str` alive through the
struct's `child_objs`; mutsu has no such back-reference, so the strings are
interned by content and live for the rest of the process — bounded by the number
of *distinct* strings a program writes into struct fields rather than by the
number of writes, the same trade `native_object_where` already makes for `.WHERE`
blocks.

Alongside it, `.^array_type` — the element type of a native array-ish container —
which did not exist at all (`No such method 'array_type'`):

| | raku | mutsu now |
| --- | --- | --- |
| `Buf.new(1,2,3).^array_type` | `uint8` | `uint8` |
| `Buf[uint64].new(1,2).^array_type` | `uint64` | `uint64` |
| `'ab'.encode('utf8').^array_type` | `uint8` | `uint8` |
| `CArray[int32].new.^array_type` | `int32` | `int32` |
| `array[uint8].^array_type` | `uint8` | `uint8` |
| `Str.^array_type` | `Mu` | `Mu` |

It is derived from the name `.^name` reports (`dispatch_caret_name`, which is
where a `CArray[int32]` gets its parameterised spelling from the container
metadata), so the two cannot disagree.

Both are [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)'s
**P0**: the pieces of the `NativeHelpers::Blob` path that are ordinary NativeCall
compatibility bugs rather than representation work. `NativeHelpers::Blob` asks
every container it is handed for `.^array_type` and feeds the answer to
`nativesizeof` and `nativecast(Pointer[T], …)`; `DBDish::mysql` fills a
`MYSQL_BIND` by assigning to its fields through exactly such a handle. Neither
needs anything from P1-P3.

One thing worth recording, because it explains a declaration that looks like a
mistake: **Rakudo cannot assign through a `Pointer`-typed CStruct accessor** — it
dies with "Cannot modify an immutable `NativeCall::Types::Pointer` type object".
That is why `DBIish`'s `MYSQL_BIND` declares `has intptr $.buffer is rw` with the
`Pointer` version commented out directly above it, and why the test pins the
integer-field form.

Pinned by `t/nativecall-cstruct-field-write.t`, which passes identically under
`raku`.
