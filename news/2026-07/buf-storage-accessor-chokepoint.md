# `Buf`/`Blob` element storage has a single accessor chokepoint

A `Buf` has no dedicated `Value` variant: it is a `Value::Instance` whose one
attribute, `"bytes"`, holds a `Value::Array` with one boxed `Value::Int` per
element. Until now every place that touched that storage spelled the attribute
name itself and open-coded the `ValueView::Array` match — **104 touches across
~40 files**, with no centralised accessor anywhere. That is what made ADR-0015
P2 (a native-backed contiguous buffer, so `Buf.REPR` can honestly answer
`VMArray` and NativeCall can hand C a real `MVMArrayB` body) a forty-file change
rather than a one-file change.

The new `src/value/value_buf.rs` is that chokepoint. The attribute name is
`const ELEMS_ATTR`, private to the module, and all 104 touches now go through
one of its functions. Two levels are offered deliberately:

- the **element** functions (`buf_elems`, `buf_elems_or_empty`, `buf_elems_in`,
  `with_buf_elems`, `with_buf_elems_mut`, `set_buf_elems`, `store_buf_elems`,
  `buf_attrs`, `make_buf`, `make_buf_from_u8`, `bytes_to_elems`) decode to and
  from `Vec<Value>`; under P2 they become the encode/decode boundary;
- the **storage** functions (`buf_storage`, `set_buf_storage`,
  `buf_elems_as_array`) move the container across *without* decoding it, for the
  coercions that only re-tag a buffer (`.Buf`, `.Blob`, `.List`); under P2 they
  become a node share.

`buf_elems` deliberately returns `Option`, because "no element storage at all"
(a `Blob` **type object** — `$*DISTRO.signature`) and "an empty buffer" are
different things at several call sites, including the string-interpolation arm
that decides whether to raise `X::Buf::AsStr`. `has_buf_elems` is the probe for
that distinction.

This is a pure refactor: no behaviour changes, and it is smaller than what it
replaced (45 files, +358/-597). Two nuances were worth preserving rather than
tidying away:

- The three byte-decoding conventions in the tree (truncating `as u8`,
  `.clamp(0, 255) as u8`, and `to_f64() as u8`) are **not** unified — that would
  be a behaviour change, so each caller keeps its own decode over a borrowed
  element slice from `with_buf_elems`.
- `.List`/`.list`/`.Array` on a Buf shares the backing array node rather than
  copying it. `buf_elems_as_array` keeps that share. It is invisible either way,
  because element writes go through `with_buf_elems_mut`, whose `Gc::make_mut`
  forks a shared node — but copying would have added an allocation to every
  such coercion.

The two remaining `"bytes"` literals outside the module are the Raku **method**
name `.bytes` in dispatch tables, not attribute keys.

This is step 1 of the three-step slicing in
[`todo/deep/adr0015-p2-buf-storage-survey.md`](../../todo/deep/adr0015-p2-buf-storage-survey.md).
Steps 2 (the node) and 3 (the `MVMArrayB` body and the honest `.REPR`) are where
the design judgment lives, and they are now changes to one file plus the GC
plumbing rather than to forty callers.

Pin: `src/value/value_buf.rs` unit tests (4), covering the round trip, the
absent-vs-empty distinction, in-place mutation seen through an alias, and the
storage hand-off.
