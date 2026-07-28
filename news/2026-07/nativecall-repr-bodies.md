# `BODY_OF` works: honest `.REPR` and a real `.WHERE` for NativeCall handles

`MoarVM::Guts::REPRs`' `BODY_OF` — how every `NativeHelpers` entry point gets at
a container's element pointer — now works on mutsu for a `nativecast`ed handle:

```raku
my $r = nativecast(Rec, $blk);
say BODY_OF($r).cstruct.Int;    # the address of the struct, on both implementations
```

It needs two things that mutsu did not answer honestly. `.REPR` said `P6opaque`
where raku says `CStruct`, so the module refused the object outright; and `.WHERE`
was a hash of the object's identity, which is fine until something *dereferences*
it.

## The two travel together, and the order matters

`BODY_OF` dispatches on `.REPR` and then dereferences `.WHERE`, so answering
`.REPR` honestly is a **promise that a REPR body sits at `.WHERE`**. Doing them
in the wrong order is not a cosmetic bug: with `.REPR` fixed and `.WHERE` still an
identity hash, the module read wild memory and mutsu **segfaulted**. That is the
hazard [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
§2.1 names as its safety rule, and it was measured, not hypothesised.

So the rule is enforced by construction: only an object whose whole identity *is*
a C address — a `nativecast`ed CStruct, CUnion or CArray — gets the honest name,
and it gets a body in the same breath.

## The body needed no new machinery

mutsu's `.WHERE` contract, fixed when `NativeHelpers::Blob` was first made to
load, is "points straight at the payload, no object header" — the module's probe
computes `Offset` as 0. And `native_object_where` already hands out a zero-filled
block whose first word is the address. That block *is* the CStruct body
(`{void* cstruct; void** child_objs}`), byte for byte, and it is also the CArray
body (`{void* storage; void** child; i32 managed; i32 allocated; i32 elems}`) for
an unmanaged cast: storage set, `managed` and `elems` zero — exactly what an
unmanaged `CArray` handle is.

## What deliberately still says `P6opaque`

A CStruct constructed in Raku, and every `Buf`. They have no C storage yet, so
claiming `CStruct`/`VMArray` would promise a body that is not there and `BODY_OF`
would quietly read a NULL one instead of refusing. Under-reporting keeps it loud.
Giving those objects real storage — and with it the honest name — is ADR-0015's
P2/P3.

## Found on the way

A `nativecast` through a **qualified** body type produced an unusable handle:
`Pointer[MoarVM::Guts::REPRs::CStructB]` was shortened by splitting on the last
`::` of the whole string, yielding the nonsense class `CStructB]`. Shortening now
leaves the type argument alone. The same fix applies to a field type written
inside a module, which arrives carrying that module's package
(`MoarVM::Guts::REPRs::Pointer[Pointer]`) and so was not recognised as a pointer
at all — which, since an unmarshallable field aborts the whole layout by design,
meant the body struct had no layout and none of its fields could be read.

## What is left before `LinearArray`

ADR-0015 named `NativeHelpers::CStruct`'s `LinearArray` as P1's acceptance. The
REPR-body mechanism is done and verified against the real module, but two
unrelated bugs still stand between it and `LinearArray`, neither of them
NativeCall work: a punned *parameterised* role never runs its `BUILD`, and a `my`
in a role body initialised from the role's type parameter reads as 0. Both are
reduced to a few lines in
[`todo/tickets/parameterised-role-pun-skips-build.md`](../../todo/tickets/parameterised-role-pun-skips-build.md).

Pinned by `t/nativecall-repr-body.t`. Its body-reading half scans for the body
the way the module does rather than assuming an offset, so it passes identically
under `raku`; the three assertions that pin mutsu's deliberate under-report are
marked as such.
