# `CArray[T]` stops being a copy and becomes the memory C writes

A `CArray[T]` was an `Array` of boxed elements, and every native call built a
fresh C block from it on the way in and read the block back on the way out. That
copy is correct only for a callee that writes into the buffer *during* the call.
`NativeHelpers::Blob`'s managed `carray-from-blob` does this instead:

```raku
my \arr = CArray[t].new;
arr[$bb.elems - 1] = 0;             # force allocation
memcpy(BODY_OF(arr).storage, $bb.realstart, …);   # write, later, through the address
```

There is no call boundary between taking the address and the write, so there was
no point at which a copy could have been synced back — and no address to take in
the first place: a Raku-side `CArray`'s `.REPR` was `P6opaque` and its `.WHERE`
was a hash of its identity.

A `CArray` over a **native numeric** element type is now a storage-backed
instance, exactly as a `Buf` has been since
[P2](buf-native-byte-node.md): its elements are contiguous bytes in a payload-only
[`BufData`] node, `.REPR` answers `CArray`, `.WHERE` answers a `CArrayB` block
describing that storage, and a native call is handed the array's own bytes. This
is [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
P3a.

## What made it small

P2 had already built everything except the class-name filter. The node, the
encode/decode boundary, the in-place write discipline (write *through* an unshared
node so an address stays valid), and the whole `value_buf` accessor layer are
shared verbatim; `value_carray` adds ~40 lines — the filter, construction, and the
`CArrayB` body — and `is_native_elems_class` widens the dozen element-storage
gates (indexing, assignment, `.elems`, `.list`, `.of`, iteration) that used to say
`is_buf_or_blob_class`.

Two things had to generalise. The node's element descriptor was a `signed: bool`,
which cannot express `CArray[num64]`, so it became an `ElemKind` of
`Uint`/`Int`/`Float`. And four hand-inlined copies of the Buf class-name ladder —
`.list`, `.List`, `.Capture`, and the one iteration methods use — were collapsed
into the shared predicate on the way past.

## Reference-typed elements deliberately keep the boxed form

`CArray[Str]`, `CArray[Pointer]`, a nested `CArray[CArray[…]]` and a CStruct
element are **addresses of other objects**. Their bytes are a pointer, and reading
one back means materialising the object it points at, which contiguous bytes alone
cannot do — MoarVM keeps a parallel `child` table for exactly this. Those keep the
`Array` representation and the per-call `char**` build, and go on under-reporting
`.REPR` as `P6opaque`, which is the safe direction: ADR-0015 §2.1's ordering rule
is that an honest `.REPR` is a promise that a body exists behind `.WHERE`. A
node-backed array therefore never has children, and `CArrayB.child` is always
NULL.

## Three general bugs found behind the same two test files

None is about `CArray`; all three were blocking `NativeHelpers::Blob`'s
`03-pointer.t`, which is `NativeHelpers::Pointer`'s whole test suite.

**A list infix is looser than the argument comma — except in a `.= new:` list.**
`my CArray[uint16] $a .= new: 10, 20 ... 100` built `10, 20, 21, 22, …`: the
colon-argument list was parsed one argument at a time, so the sequence's seed was
only its *last* element (`10, (20 ... 100)`) instead of the whole comma level.
The ordinary postfix `.method: …` path had had this lift for a while; the four
`.=` sites (declaration, assignment statement, `has` default, `constant`) each
carried their own byte-identical copy of the argument loop, which is how all four
came to be missing it. They share one implementation now, and it lifts the `Z`/`X`
meta-ops too.

**`^add_method` on a qualified class name added to a stub nobody consults.**
`NativeHelpers::Pointer` builds all of NativeCall's pointer arithmetic with
`NativeCall::Types::Pointer.^add_method('add', …)`, while the prelude registers
`Pointer` under its short name and tags every handle with it. The long name was
not a registered class, so `add_method` created a fresh stub, populated it, and
nothing ever looked there: `.add` was "no such method" and `.succ`/`.pred` fell
through to the *numeric* successor, advancing a `Pointer[uint16]` by one byte
instead of one element.

**An added method's invocant was counted as a parameter.** `method (Pointer:D:
Int $off)` filtered the invocant out of `param_defs` but not out of the positional
name list dispatch binds against, so every argument shifted by one and the last
parameter was unbound — `Variable 'off' is not declared`.

Plus one parity gap in the same file: `isa-ok $p.succ, Pointer[uint16]` failed
because a typed pointer keeps its parameterisation in an `of` attribute rather
than in its class name (deliberately, so every `Pointer` method keeps working), and
the type check never looked there.

## Results

`NativeHelpers::Blob`'s `01-basic.t` (24/24, was 8/24) and `03-pointer.t` (10/10,
was 0/10) joined the battery gate, closing the last two files of
[issue #5557](https://github.com/tokuhirom/mutsu/issues/5557) that could be
closed — `02-cstruct.t` is not whitelistable at all, because raku itself fails its
tests 13 and 15 on this machine. The unmanaged-`CArray` gap noted in that issue is
fixed on the way past: a `nativecast`ed handle has no length to report, so
`.elems` throws Rakudo's "Don't know how many elements a C array returned from a
library" instead of answering 0.

Parity gained beyond the two files: growing a `CArray` by element assignment
zero-fills (it left `Any` holes), `.list` is a `List` rather than an `Array`, and
`CArray[num32]`/`CArray[num64]` elements read back as `Num`s.

`array[T]` is **not** part of this — it is P3b, and it needs a different first
step, because it is a `Value::Array` rather than an instance, so its
`ArrayData::items` touches need the accessor chokepoint P2 built for `Buf`'s.

Pins: `t/carray-native-storage.t`, `t/colon-args-list-infix-precedence.t`,
`t/add-method-qualified-and-invocant.t`.
