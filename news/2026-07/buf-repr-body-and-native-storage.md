# `Buf` answers `VMArray` and hands C its own storage

[ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
P2 is complete. A `Buf`/`Blob` now reports its REPR honestly, carries a
synthesised MoarVM `MVMArrayB` body at its `.WHERE`, and is passed to a native
call as a pointer into **its own bytes** rather than into a mirror of them. The
mirror — `runtime/nativecall_pin.rs`, whose own `TODO` said the real fix was a
native representation — is deleted, along with the copy-back loop it existed to
feed.

This is what `NativeHelpers::Blob`'s `pointer-to` needs. It dispatches on
`.REPR` and then dereferences `.WHERE`, so the two had to become true in the
same commit (ADR-0015 §2.1's ordering rule: an honest name with no body behind
it is a segfault, not a small inaccuracy). End to end, on this build:

```raku
use NativeHelpers::Blob;
my $b = Buf.new(1, 2, 3);
my $p = pointer-to($b);          # the buffer's own element address
memset($p, 65, 3);               # C writes, with no call that mentions $b
say $b.decode;                   # AAA
```

The last line is the point. `DBDish::mysql` stores a buffer's address into a
`MYSQL_BIND` struct and lets `mysql_stmt_fetch` fill it later; the buffer is
never an argument of the call that writes it, so there is no boundary at which a
mirror could have been copied back. Shared memory has nothing to keep in sync.

## What the body is

`{u64 elems; u64 start; u64 ssize; void* any}`, laid out exactly as
`MoarVM::Guts::REPRs` declares it, allocated once per storage node and refreshed
in place — so a C structure that captured the address keeps reading a live
element pointer across a reallocation, which is ADR-0015 §2 contract 3. It could
not reuse the block `native_object_where` hands out for CStruct and CArray
handles: that one is memoised by payload address, immutable, and permanently
leaked, which is enough only because those two bodies are all-zero past their
first word. Three of `MVMArrayB`'s four words are live.

Ordinary Raku writes into a buffer now go *through* the existing storage
whenever the instance is its only holder, instead of swapping in a fresh node.
A shared node — `.Buf`/`.Blob` re-tag one buffer's storage under another name
without copying it — is still replaced, so Raku's copy semantics are unchanged.
The effect is that a pointer C is holding survives an ordinary element write,
and only growing past the allocation invalidates it: the same contract Rakudo's
`VMArray` offers, no more.

The compatibility surface ADR-0015 §4 promised is written down in
[docs/nativecall-repr-bodies.md](../../docs/nativecall-repr-bodies.md), pinned by
`t/nativecall-repr-body.t`, which declares the body structs the way the module
does and derives the offset by scanning rather than assuming.

## A CStruct field named like a builtin, inside a module

Getting `BODY_OF` to actually read the body turned up a second bug, and it is
the more general of the two. `nativecast` tags a handle with the class's *short*
base name, but the accessor lookup on the method fast path used that name to
find the attribute — so a CStruct declared inside a module
(`MoarVM::Guts::REPRs::MVMArrayB`, reached as `MVMArrayB`) found no attribute
and fell through to the builtins.

Only field names that also name a builtin method ever noticed, which is why it
had gone unseen: `.start` and `.ssize` read the struct correctly while `.elems`
answered `1` and `.any` built a Junction, and `NativeHelpers::Blob` came away
with `any(MVMArrayBody.new(elems => 1, …))` instead of a body. The lookup now
goes through the registered name. Pinned by two new cases in
`t/nativecall-cstruct-fields.t`.

## Not yet: `DBIish`'s mysql driver

The original acceptance test for P2 was `DBIish` `01-basic` reaching raku parity
(35/35). It does not, and P2 is no longer why: `BODY_OF` works, and
`DBDish::mysql::StatementHandle` now fails earlier, in the *parser*. A bare
identifier in a ternary's then-branch is rejected unless the parser can tell it
is a complete term rather than a listop head, and an enum value imported from
another module is not something it can currently tell — so
`.buffer_type = @!column-type[$col] ~~ Blob ?? MYSQL_TYPE_BLOB !! MYSQL_TYPE_STRING`
does not parse. Recorded as
[todo/tickets/ternary-then-branch-enum-value.md](../../todo/tickets/ternary-then-branch-enum-value.md).
