# `Buf.push` encodes only the new elements, so filling a buffer is linear again

Assembling a buffer one element at a time — what every binary-protocol routine
does, from `HTTP::HPACK`'s Huffman `decode-str` to Cro's frame serializers — was
quadratic in the buffer's length. `$buf.push($byte)` cost time proportional to
how many bytes `$buf` already held, so the cost of filling an n-byte buffer went
as n².

## What was happening

A `Buf`/`Blob` stores packed bytes in a `BufData` node, which is the right
representation. But `push`/`append` reached it through the *element* level of
`value_buf.rs`'s accessors — the encode/decode boundary that hands out
`Vec<Value>` — and so round-tripped the **whole buffer** on every call:

1. `buf_elems_or_empty` decoded the storage into one boxed `Value::Int` **per
   existing byte**;
2. the new items were appended to that `Vec<Value>`;
3. `buf_attrs` re-encoded all n+k elements into a fresh `Vec<u8>`;
4. `write_back_sharing` rebuilt the instance's whole attribute map around it.

Steps 1 and 3 are O(n) per push, and only step 2 has anything to do with what
the caller asked for. The rvalue path (`Buf.new($x).append: $y`, which has no
container to write back into) did the same round trip.

## What it does now

The module's doc comment already promised a **byte** level of accessors that
"speak what the node stores, so a caller that only wants bytes or a count never
round-trips through boxed `Value`s". `push`/`append`/`unshift`/`prepend` have
joined it. `extend_buf_elems` reads the element width and kind off the node,
encodes **only the new elements** at that width, and appends those bytes onto
the node's storage in place — the same audited unshared-node write
`put_bytes` performs for `$b[i] = v`, which ADR-0013's `GcBox`/`UnsafeCell`
interior mutability makes sound. The existing bytes are never read. A
`Back` push is therefore amortized O(k) in the number of elements pushed,
independent of the buffer's length; `Front` still shifts the existing bytes, as
any prepend onto contiguous storage must, but it too no longer decodes them.
The rvalue path gets `buf_attrs_extended`, the same encode-the-new-elements-only
construction against a fresh attribute map.

The write goes through the instance's shared attribute cell, so an alias sees the
push — `my $a = Buf.new(1,2); my $b = $a; $a.push(3)` leaves `$b` with three
elements, because both names denote the same mutable `Buf` object, which is what
Raku means. Storage *shared by a re-tagging coercion* (`.Buf`/`.Blob`, which move
one buffer's node under another name without copying it) is forked instead, so a
push through one tag is not seen through the other.

## Measurements

Release build, 500 iterations each, this container (`raku` is v2026.07):

| | before | after | rakudo |
| --- | --- | --- | --- |
| `Buf.new` + 10 pushes | 0.078 ms | 0.075 ms | 0.043 ms |
| `Buf.new` + 100 pushes | 0.715 ms | 0.622 ms | 0.009 ms |
| 10 pushes onto a 200-byte buf | 0.411 ms | 0.075 ms | 0.003 ms |
| 10 pushes onto a 1000-byte buf | — | 0.059 ms | 0.003 ms |
| 10 pushes onto a 4000-byte buf | — | 0.059 ms | 0.003 ms |

The last three rows are the point: pushing 10 elements now costs the same
whether the buffer holds 200 bytes or 4000, where before it scaled with the
length (a 200-byte buffer already cost 5x an empty one). The 5.5x on the
200-byte row is what a *small* buffer recovers; the win grows without bound with
the buffer.

What is left is a flat per-push constant of about 7 µs, roughly 16x an
`Array.push`, and it is not buffer work at all: a callgrind profile of a
push-only loop attributes it to `type_matches`, class-name substring searching
and `composed_roles_seed` — the instance method-dispatch path in front of the
buffer code, not the buffer code itself. That is filed separately as #7696.

## A truncation the round trip caused

Encoding at the node's own width also fixed a data-corrupting bug the element
round trip had. `.Buf`/`.Blob` move a buffer's storage node under another class
name without decoding it, so `buf16.new(0x1234, 0x5678).Buf` is an instance
whose *class* says width 1 while its *node* is width 2. The old push re-encoded
the decoded elements at `elem_type(class_name)` — width 1 — and silently
truncated them:

```
# before
my $c = buf16.new(0x1234, 0x5678).Buf;
say $c.raku;      # Buf.new(4660,22136)
$c.push(9);
say $c.raku;      # Buf.new(52,120,9)     <-- both existing elements truncated

# after (and the element values Rakudo gives)
$c.push(9);
say $c.raku;      # Buf.new(4660,22136,9)
```

(mutsu still names that buffer `Buf` where Rakudo names it `Buf[uint16]` — a
separate gap in the `.Buf` coercion, untouched here.)

## A Blob that could be mutated

`Blob` is immutable, and the rvalue path, `pop`, `shift` and `splice` all refuse
to modify one. The named-variable `push`/`append` path never checked: `my $blob =
Blob.new(1,2); $blob.push(3)` silently grew it to three elements instead of
dying. Since the write it performs is now explicitly in place, the missing check
went in alongside — `t/buf-push-linear-growth.t` pins it, together with argument
flattening, width-preserving encoding for `buf16`, element truncation, the
`X::TypeCheck` on a `Str` element, the alias and `.Blob`-fork semantics above,
and a 20,000-element fill loop that is a fraction of a second under the
byte-level path and minutes under the quadratic one.
