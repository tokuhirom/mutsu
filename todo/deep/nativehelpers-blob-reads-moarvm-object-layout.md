# `NativeHelpers::Blob` reads MoarVM's internal object layout

`DBIish`'s `$dbh.prepare(...)` dies with

```
Cannot dereference a Pointer[Any]: not a type NativeCall can read
  in sub protect-connection ...
  in sub prepare ...
```

`DBDish::mysql`'s prepared-statement path binds parameters through
`NativeHelpers::Blob`'s `pointer-to` / `carray-from-blob`, and every one of those
goes through `BODY_OF` in the bundled `MoarVM::Guts::REPRs`:

```raku
constant Offset = do {
    my Pointer \p = Pointer.new(0xdeadbeaf);
    my CArray[intptr] \ar = nativecast(CArray[intptr], Pointer.new(p.WHERE));
    my $i = 0;
    repeat { last if ar[$i] == p; } while ++$i < 10;
    $i * ptrsize;
};
sub OBJECT_BODY(Mu \any) is export { Pointer.new(any.WHERE + Offset) }
sub BODY_OF(Mu \any) is export {
    my \type = %known-bodies{any.REPR};      # VMArray => MVMArrayB, CArray => ..., CStruct => ...
    nativecast(Pointer[type], OBJECT_BODY(any)).deref;
}
```

That is not a NativeCall API — it walks **MoarVM's own object header** with a
probed offset and reinterprets the result as an `MVMArrayB` / `CArrayB` /
`CStructB` `CStruct`. The module's own header calls itself "incomplete,
undocumented and mainly a proof of concept". mutsu's object representation is
not MoarVM's, so running this source can never produce a correct answer: right
now `%known-bodies{any.REPR}` misses, `type` is undefined, and the `nativecast`
to `Pointer[Any]` is what actually raises.

## Why this is deep

The fix cannot be "make the source work". It has to be a **native override**: mutsu
must supply its own implementations of the `NativeHelpers::Blob` surface that
DBIish (and anything else in the ecosystem) actually calls —

- `pointer-to(Blob:D | array:D | CArray:D, :$typed)`
- `BPointer`, `ptr-sized`, `sizeof`, `buf-sized`
- `carray-from-blob`, `carray-is-managed`, `blob-allocate`
- `blob-from-pointer`, `utf8-from-pointer`

— backed by mutsu's own buffer representation, and shadow the community module's
`MoarVM::Guts` route rather than execute it. mutsu already grows NativeCall this
way elsewhere (`CType::Buf` writeback, CStruct opaque pointers), so the machinery
exists; the open questions are where the override lives (a bundled replacement
module vs. native builtins registered under the module's exported names), how it
interacts with `-I` precedence when a user pins the real distribution, and which
of these need to hand out a pointer that stays valid after the call returns.

## Repro

```
cd tmp/dbslot/DBIish-0.6.8
../../../target/debug/mutsu -I lib -I ../NativeLibs-0.0.9/lib \
    -I ../NativeHelpers-Blob-*/lib ../../dbiish-prep.raku
```

(needs the `mutsu-mariadb` container on port 13306). Connecting and `.execute`
already work end to end against the live server; `prepare` is the first call that
reaches `BODY_OF`.

## Impact

Last known blocker on `DBIish`'s real end-to-end mysql path, and the gate on
prepared statements for any NativeCall binding that uses `NativeHelpers::Blob`.
The three earlier blockers are fixed:
`news/2026-07/rw-sub-proxy-fetch-on-otf-call.md`,
`news/2026-07/closure-capture-beats-same-named-caller-lexical.md`,
`news/2026-07/module-type-aliases-outlive-the-requiring-frame.md`.
