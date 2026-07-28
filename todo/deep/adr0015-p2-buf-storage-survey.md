# ADR-0015 P2 (native-backed `Buf`/`Blob`) — implementation survey

P2 is the only thing left between mutsu and `DBIish`'s mysql driver: `BODY_OF`
runs, and stops on `Buf.REPR` answering `P6opaque` where raku answers `VMArray`
(see the ⑨ row of [`../tickets/dbiish-blockers.md`](../tickets/dbiish-blockers.md)).
This file is the measured survey of what P2 actually costs, taken 2026-07-28
against `main`. It **refines** [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
§6; it does not change any decision in it, so the ADR is left as `Accepted`
rather than superseded. Three of the numbers and one of the mechanisms in §6
turn out to be wrong, and the third one is the reason this is written down.

## 1. `Buf`/`Blob` today

There is no dedicated `Value` variant. A `Buf` is a plain `Value::Instance`
(`src/value/mod.rs:1213`) with one attribute, `"bytes"`, holding
`Value::array(Vec<Value>)` — **one boxed `Value::Int` per element**. Built by
`build_native_buf_value` (`src/runtime/methods_object_native_ctors_buf_num.rs:77`).

**Element width is not stored anywhere in the data.** It lives only in the class
name string, and is recovered by matching on it: the construction-time mask is
`cn.contains("64")` / `("32")` / `("16")` (`methods_object_native_ctors_buf_num.rs:56-74`),
and `.^array_type` re-derives it in `array_element_type_name`
(`src/runtime/methods_classhow_dispatch.rs:36-49`). The new node has to carry it,
because a contiguous `Vec<u8>` cannot be interpreted without it.

**Status: step 1 of the slicing below is DONE** — the accessor chokepoint is
`src/value/value_buf.rs` and all 104 touches route through it
(see [`news/2026-07/buf-storage-accessor-chokepoint.md`](../../news/2026-07/buf-storage-accessor-chokepoint.md)).
Sections 2 and 5 below are now historical: read them for *why* the campaign was
needed, not as work still to do. Step 2 (the byte view and the width probe) is
done too; steps 3 and 4 are open.

## 2. The `"bytes"` campaign is 104 sites, not 91 (done — historical)

`grep -rn '"bytes"' src/`: **110 occurrences in 45 files**, of which 6 are not
attribute touches (method-name literals and comments). The real total is **104
across ~40 files**:

| kind | count | pattern |
| --- | --- | --- |
| reads | 63 | `.get("bytes")` |
| writes | 38 | `"bytes".to_string()` as an insert key |
| in-place | 2 | `with_attr_mut("bytes", …)` (`src/vm/vm_var_assign_index_named.rs:845,868`) |
| probe | 1 | `contains_key("bytes")` (`src/vm/vm_var_assign_typed.rs:611`) |

The ADR's "~91" matches the read count exactly (63) but undercounts writes (28
vs the actual 38). The heaviest single file is
`src/runtime/methods_mut_substr_buf.rs` (15). There is **no existing centralised
accessor** — nothing named `buf_bytes`/`blob_bytes` exists; the nearest thing is
`socket_helpers::extract_bytes`, scoped to `crate::runtime`.

The touches are highly uniform, which is what makes the migration mechanical:
reads are `attributes.as_map().get("bytes").map(Value::view)` matched against
`ValueView::Array(items, ..)`, writes are
`attrs.insert("bytes".to_string(), Value::array(vals))`. The companion class-name
filter already exists — `runtime::utils::is_buf_or_blob_class`, 50 call sites.

## 3. `.REPR` already has real machinery (P1 landed)

`.REPR` is no longer a constant. `Interpreter::try_native_handle_repr_where`
(`src/runtime/cstruct_layout.rs:603-647`) answers **CStruct, CUnion and CArray**
honestly today, with `Pointer` handled for `.WHERE`. P2 extends that function; it
does not start from scratch.

Two details to keep in mind when extending it:

- It is reached from **two** call sites which must stay in step —
  `src/runtime/methods_call_dispatch.rs:68` and `src/vm/vm_native_dispatch.rs:51`
  (whose comment records that doing it the other way round segfaults).
- Its current gate keys off an `address` **attribute** being a positive `Int`,
  i.e. a `nativecast`ed handle. A native-backed `Buf` has no such attribute, so
  it needs a different discriminator — either the node's presence, or an
  `address` synthesised from it.

Everything else falls through to `"P6opaque"`
(`src/runtime/methods_instance_ops.rs:1917`).

## 4. ★ `native_object_where` cannot be extended into `MVMArrayB`

This is the finding that matters, because ADR-0015 §6 describes P1's body block
as "the shape `native_object_where` already established" and implies P2 extends
it. It cannot. `native_object_where` (`src/runtime/nativecall.rs:409-424`) is:

- **memoised by payload address** in a process-global map, so two objects at the
  same address deliberately share one block;
- **immutable after creation** — `vec![0usize; 16]` with only word 0 set;
- **`Box::leak`ed permanently**, never freed.

It satisfies P1 only because the CStruct body (`{void* cstruct; void** child_objs}`)
and the unmanaged CArray body (`{void* storage; void** child; i32 managed; i32
allocated; i32 elems}`) are both all-zero past word 0 — so one shared zero block
is byte-identical to both (the reasoning is written out at
`src/runtime/cstruct_layout.rs:590-599`). There is no `CStructB`/`CArrayB` Rust
struct anywhere; the layouts exist only as Raku declarations in
`t/nativecall-repr-body.t:19-30`.

`MVMArrayB` is `{u64 elems; u64 start; u64 ssize; void* any}` — **three live
non-zero words plus a data pointer**, all of which change when the buffer is
appended to or reallocated. So P2 needs a **per-object, mutable, owned** block:

- per-object, because sharing by payload address is wrong once the words differ
  (and buffer addresses are reused after a free);
- mutable, because ADR-0015 §2 contract 3 promises the body block stays put and
  its data pointer is rewritten on realloc;
- owned, because leaking 128 bytes per `Buf` is not acceptable — the block must
  die with the node.

The upside is that owning it gives a natural home for the teardown that
`nativecall_pin`'s `release` hook currently performs from
`impl Drop for InstanceAttrs`.

## 5. Retiring `nativecall_pin.rs`

109 lines, **4 non-test callers**:

- `src/runtime/nativecall.rs:765` — `pin`, in the `CType::Buf` marshalling arm;
- `src/runtime/nativecall.rs:361` — `read`, in the post-call writeback loop;
- `src/value/value_instance.rs:342` — `release`, from `impl Drop for InstanceAttrs`.

Three helpers in `nativecall.rs` go with it: `buf_instance_bytes` (`:800`, the
boxed→`Vec<u8>` copy), `buf_instance_pin_key` (`:826`) and
`write_buf_instance_bytes` (`:843`). Deleting the `release` call also removes an
atomic load from every instance drop.

`t/nativecall-buf-lifetime.t` (7 tests) is the behavioural pin — "the same Blob
keeps the same C address across calls", "a distinct Blob gets a distinct C
address". Both become *stronger* under P2, so the file should survive unchanged;
if it needs editing, that is a signal the representation is wrong.

## 6. Where the new GC node goes

Nine `Gc<T>` node types exist today, enumerated in the nanbox refcount dispatch
(`src/value/nanbox/mod.rs:433-449`) and the collector's scope comment
(`src/gc/collect.rs:17-20`). A payload-only node holds no `Value`s, so
`Trace::trace` is an empty body and `drop_gc_edges` / `finalize` keep their
no-op defaults (trait at `src/gc/gc_ptr.rs:83-116`) — which is exactly what makes
ADR-0001's type filter keep paying zero for it. `impl Trace for ArrayData`
(`src/value/value_gc.rs:399`) is the shape to copy.

Files a new kind must touch:

| file | what |
| --- | --- |
| `src/value/mod.rs:1155` | new `ValueRepr` variant in the Gc-backed group |
| `src/value/view.rs:25` | matching `ValueView` variant |
| `src/value/nanbox/mod.rs:144` | `Kind` discriminant **inside the Gc-backed block**; the page budget assert at `:181` has headroom |
| `src/value/nanbox/mod.rs:433` | the `gc_op::<T>` refcount arm |
| `src/value/nanbox/{encode.rs:58,decode.rs:211}` | pack / unpack |
| `src/value/nanbox/peek.rs:172,246,458` | the three borrowed-view tables |
| `src/value/value_gc.rs` | `impl Trace` (empty) and the `Value::gc_trace` arm |
| `src/value/serde_support.rs` | the exhaustive `ValueRepr` match |

Writes into the buffer use ADR-0013's `GcBox` interior mutability so they have
valid provenance.

## 7. The acceptance test already exists and says what to flip

`t/nativecall-repr-body.t` (15 tests) declares Raku-side `CStructB`/`CArrayB`
byte-identical to `MoarVM::Guts::REPRs`, and derives the body offset by scanning
for a sentinel exactly as that module derives `Offset` — it does not assume 0.
Its last assertion is the one P2 inverts:

```raku
is Buf.new(1, 2, 3).REPR, 'P6opaque', 'a Buf has no body yet either';
```

P2 must, **in one commit** (ADR §2.1's ordering rule), add an `MVMArrayB`
declaration plus its offset probe, flip that assertion to `'VMArray'`, and bump
`plan 15`. `docs/nativecall-repr-bodies.md` — the compatibility-surface document
ADR §4 promises — does not exist yet and is part of P2.

## Suggested slicing

1. ~~**Accessors first, no representation change.**~~ **DONE.**
   `src/value/value_buf.rs` owns the attribute name and all 104 touches go
   through it. Two levels landed, not one: *element* accessors that decode to
   and from `Vec<Value>` (these become the encode/decode boundary in step 2) and
   *storage* accessors that move the container across without decoding it (these
   become a node share). `buf_elems` returns `Option` because "no storage at
   all" (a `Blob` type object) and "empty buffer" are distinguished at several
   call sites, `has_buf_elems` being the probe.
2. **The byte view and the width probe.** **DONE** — see
   [`news/2026-07/buf-byte-and-width-accessors.md`](../../news/2026-07/buf-byte-and-width-accessors.md).
   `value_buf` now also answers in bytes (`buf_bytes`, `with_buf_bytes`,
   `set_buf_bytes`, …) and in counts (`buf_len`), which is what the majority of
   the element-accessor callers actually wanted; and `buf_elem_width` replaced
   the four separate `cn.contains("16")` ladders, so the element width — the one
   part of a `Buf`'s type that is not in its data at all — is derived in one
   place ready to move into the node.

   The three byte-decoding conventions are resolved to **truncation**: it is
   what Raku itself stores by (`Buf.new(300)` is `0x2C`, `Buf.new(-1)` is
   `0xFF`), every mutation path masks on the way in, and for a wider buffer
   Rakudo agrees with neither convention anyway.

3. **The node**, behind those accessors: contiguous `Vec<u8>` + element width,
   with the encode/decode the accessors now hide. Signedness has to ride along
   with the width — mutsu stores every element unsigned today, so
   `Blob[int8].new(-1)[0]` answers `255` where raku answers `-1`, and the node
   is where that stops being true.
4. **The body and the honest `.REPR`**, in one commit with the `MVMArrayB` test,
   plus the deletion of `nativecall_pin.rs` and its three helpers.

Steps 3 and 4 are where the judgment is.
