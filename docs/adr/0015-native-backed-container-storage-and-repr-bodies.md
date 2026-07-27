# ADR-0015: Native-backed container storage and synthesised REPR bodies (`BODY_OF`)

- **Status**: Accepted (2026-07-27 — tokuhirom: *make `DBIish` work even if it means changing mutsu's
  internals; move mutsu itself closer rather than building a compatibility layer that costs
  performance.* All five Open Questions resolved in §5 accordingly.)
- **Date**: 2026-07-27
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0001](0001-gc-strategy-and-phasing.md) (non-moving GC — what makes a stable address possible at all; the container type filter this ADR extends), [ADR-0013](0013-container-interior-mutability-cellvalue.md) (interior mutability — the write path into a shared container node), [ADR-0005](0005-nanbox-representation-encoding.md) (NaN-boxing — a new container kind must fit the existing tag scheme), [todo/deep/nativehelpers-blob-moarvm-guts.md](../../todo/deep/nativehelpers-blob-moarvm-guts.md) (the finding this ADR answers), [todo/tickets/dbiish-blockers.md](../../todo/tickets/dbiish-blockers.md) ⑨, PLAN.md §1 B1 (database battery) / §1 B4 (NativeCall remainder)

> This ADR decides **how mutsu hands C a pointer into a Raku container's element
> storage** — the capability behind `NativeHelpers::Blob`'s `pointer-to` /
> `BODY_OF`, and behind any binding that gives a C function an out-buffer it
> fills in later. The short answer is: by giving those containers real native
> storage and answering `.REPR` / `.WHERE` / the REPR body structs honestly,
> phased by REPR kind. It does not supersede any ADR; it extends ADR-0001's
> container-kind set with a payload-only kind and adopts ADR-0013's write
> discipline for it.

---

## 1. Context

### 1.1 Where this came from

`NativeHelpers::Blob` is a hard dependency of the **`DBIish` database battery**
(PLAN.md §1 B1), and 15 further distributions in the fez index depend on it
directly (`Archive::Libarchive`, `GD`, `GLib`, `LLVM`, `Net::BGP`,
`X11::Xlib::Raw`, …). It exists for one reason: to take a Raku `Blob` / `array` /
`CArray` / `CStruct` and produce **the address of its elements**, so a C function
can read or write them in place.

It does that by reading MoarVM's object guts. `MoarVM::Guts::REPRs` declares
hand-written `CStruct` mirrors of MoarVM's REPR body structs and casts an
object's address to them:

```raku
sub OBJECT_BODY(Mu \any) { Pointer.new(any.WHERE + Offset) }
sub BODY_OF(Mu \any) {
    my \type = %known-bodies{any.REPR};      # VMArray / CArray / CStruct
    die "Can only handle …" if type ~~ Nil;
    nativecast(Pointer[type], OBJECT_BODY(any)).deref;
}
```

The **load-time** half of this already works: `nativesizeof`, positional
`Pointer.new`, a dereferenceable `Pointer.WHERE` and reads through a `nativecast`ed
`CArray` handle all landed on 2026-07-26
([news](../../news/2026-07/nativecall-sizeof-and-pointer-where.md)), and with
them the contract that **mutsu's `.WHERE` points straight at the payload, so the
module's probe computes `Offset` as 0** — mutsu has no object header. What is
left is the half that needs real storage.

### 1.2 What is actually missing, measured

Measured 2026-07-27, debug build of `main`, both interpreters on the same input.
The two probes are kept in
[todo/deep/nativehelpers-blob-moarvm-guts.md](../../todo/deep/nativehelpers-blob-moarvm-guts.md#measuring-the-remaining-gaps-yourself):

| contract | raku | mutsu today |
| --- | --- | --- |
| `nativesizeof(T)`, `Pointer.new($addr)`, `Pointer.WHERE`, `nativecast(CArray[T], p)[i]` | ok | ✅ done (2026-07-26) |
| `Buf.new(1,2,3).REPR` | `VMArray` | `P6opaque` |
| `CArray[uint8].new.REPR` | `CArray` | `P6opaque` |
| `my array[uint8] $a .= new(…).REPR` | `VMArray` | `P6opaque` |
| `nativecast(SomeCStruct, $p).REPR` | `CStruct` | `P6opaque` |
| `Buf.new(1,2,3).^array_type` | `uint8` | `No such method 'array_type'` |
| `$buf.WHERE` | a real address | a hash of the `WHICH` identity |
| writing a field through a `CStruct` **handle** (`$s.a = 42`) | writes memory | **silently dropped** (assignment "succeeds", read-back is 0) |
| the element buffer C is handed | *is* the object's storage | a **per-call copy**, or a pinned mirror |

The last row is the substantive one. `Buf`/`Blob` is an `Instance` whose `bytes`
attribute is an `Array` of boxed `Int`s (~63 read sites and ~28 write sites touch
that attribute name directly, across ~40 files); a Raku-side `CArray[T]` is an
`Array` value tagged with an element type. Neither has contiguous C memory, so
`runtime/nativecall.rs` **copies into a temporary buffer for the duration of the
call and copies back afterwards**, and `runtime/nativecall_pin.rs` keeps a
per-object **mirror** for `Buf` so that a C function which *retains* the pointer
(OpenSSL's `BIO_new_mem_buf`) still sees live memory. `nativecall_pin.rs` carries
its own `TODO` saying the real fix is a native representation.

### 1.3 Why the mirror cannot be extended to cover this

The obvious cheap move — grow the pin registry, and copy back after each call —
**has no sync point in the case that matters**. `DBDish::mysql::StatementHandle`
does this:

```raku
@!out-bufs[$col] = blob-allocate(Buf, $!out-lengths[$col]);
.buffer = BPointer(@!out-bufs[$col]).Int;     # the Buf's address, stored into a C struct
```

The address is written **into a `MYSQL_BIND` struct**, and `mysql_stmt_fetch`
later fills the buffer. The `Buf` is never an argument of the call that writes
it, so there is no call boundary at which a mirror could be copied back, and no
way to know a write happened. A mirror here is not slow-but-correct — it is
**silently wrong**, and only under conditions a static analysis cannot see. That
is precisely the "correct only under an incomplete analysis, therefore flaky"
shape CLAUDE.md's risk definition tells us to reject.

The same pattern appears in `NativeHelpers::CStruct`'s `LinearArray`, which
`calloc`s a block, points each element at it via `nativecast`, and then relies on
`BODY_OF(...).cstruct` to get each element's address back.

### 1.4 What this is worth

- **The database battery.** `DBIish` is at 8/9 files, raku parity on 8. The last
  file (`01-basic`, 3 subtests) is the `mysql` driver, gated on exactly this
  (`todo/tickets/dbiish-blockers.md` ⑨). `DBDish::SQLite` does **not** need
  `BODY_OF`, so this is not what blocks bundling SQLite support — it is what
  decides whether the bundled `DBIish` is whole or SQLite-only.
- **15 direct dependents** of `NativeHelpers::Blob` in the ecosystem index, plus
  `DBIish`'s own 28.
- **Debt retired, not added.** It deletes the `nativecall_pin` mirror and the
  per-call `CArray` copy-in/copy-out, replacing two mechanisms with one — a "1
  operation = 1 implementation" win (PLAN.md §0 standing rule).
- **Independently a perf/memory win.** A 1 MB `Buf` currently costs a million
  boxed `Value`s in a `Vec`; the same buffer as bytes costs 1 MB and no GC
  tracing. The known native-typed shaped-array problem (`roast`
  `S09-typed-arrays/array-shapes.t` T36-38: coercion broken and ~150× slower than
  it should be) is the same missing representation, so this plausibly relieves it
  — unverified, and not a justification on its own.
- **It falsifies a standing claim.** PLAN.md §1 B4 still records that
  "`DBDish::SQLite` depends on `MoarVM::Guts::REPRs` … and cannot work in
  principle = a de-facto wall". That is now measurably false — the guts module
  loads, SQLite passes 8/9 files, and the remaining piece is a representation
  change we know how to make. The line should be corrected when this ADR lands.

---

## 2. Decision

**Adopt the REPR-body contract for real: mutsu answers `.REPR`, `.WHERE` and the
REPR body structs honestly for the four kinds `NativeHelpers` knows, backed by
genuine object-owned native storage — and roll it out by REPR kind, cheapest and
already-real first.**

The governing principle, stated by tokuhirom when approving this ADR: **move
mutsu's own representation closer to what a Raku implementation is expected to
be, rather than bolting on a compatibility layer that costs performance.** Every
fork below is decided that way — where an option "emulates" a stable buffer while
keeping the boxed representation, it is rejected even when its diff is smaller,
because the emulation is both slower and only conditionally correct.

Four contracts mutsu commits to:

1. **Offset = 0.** `.WHERE` points straight at the object's payload; mutsu has no
   object header. Already established and pinned by
   `t/nativecall-sizeof-pointer-where.t`; this ADR keeps it.
2. **The body structs mirror MoarVM's layout** for `VMArray`
   (`{u64 elems; u64 start; u64 ssize; void* any}`), `CArray`
   (`{void* storage; void** child; i32 managed; i32 allocated; i32 elems}`) and
   `CStruct` (`{void* cstruct; void** child_objs}`). mutsu commits to the
   *layout*, not to these being its real internals: they are a documented
   compatibility surface, synthesised on demand, with `start` always 0 so
   `realstart == any`.
3. **`.WHERE` is stable for the object's lifetime.** The body block is allocated
   once per object; a reallocation of the element buffer rewrites the body's
   data pointer rather than moving the body.
4. **A pointer obtained from `pointer-to` / `BODY_OF` is valid until the
   container is resized or dies** — the same contract Rakudo offers, no more.
   Non-moving GC (ADR-0001, which rejected a moving collector) is what makes even
   this much sound.

### 2.1 Phasing

Each phase is independently useful and independently shippable.

- **P0 — adjacent gaps, no bodies.** `.^array_type` on `Blob`/`array`/`CArray`
  metaobjects; **CStruct field *writes* through a native handle**
  (`cstruct_layout::write_field`, the missing mirror of the existing
  `read_field`). Both are plain NativeCall compatibility bugs today (§1.2), both
  are on the mysql path, neither depends on anything below.
- **P1 — bodies over addresses that are already real.** `CStruct` handles and
  `nativecast`ed `CArray` handles already carry a genuine C address; their bodies
  are two- and five-field structs built from it. Unblocks
  `NativeHelpers::CStruct` (`LinearArray`, `pointer-to(CStruct)`). No
  representation change.
- **P2 — native-backed `Buf` / `Blob`.** The representation change: a new
  payload-only GC node holding a contiguous `Vec<u8>` plus element-width
  metadata, held by the `Buf` instance in place of the `Array` of boxed `Int`s.
  The ~91 direct `"bytes"` attribute touches route through two accessors
  (`buf_bytes` / `buf_bytes_mut`). `.WHERE` yields the stable body block;
  `.REPR` answers `VMArray`. **Retires `runtime/nativecall_pin.rs`.**
- **P3 — native-backed Raku-side `CArray[T]` and `array[T]`.** The same node for
  the two remaining containers. **Retires the per-call copy-in/copy-out and the
  out-array writeback in `marshal_carray_arg`.**

**Ordering rule (safety-critical): `.REPR` truthfulness for a kind must land in
the same slice as that kind's body, never before it.** The moment `.REPR` says
`VMArray`, `BODY_OF` stops dying with a clear "Can only handle …" message and
starts *dereferencing whatever `.WHERE` returned*. Making `.REPR` honest ahead of
the body would hand a module the identity hash to dereference — the exact
segfault the tier-1 work already hit once (the prelude's `Pointer` is
`Foo::Pointer` inside a module, so a name-exact `.WHERE` guard fell through to
the identity hash and a binding dereferenced garbage).

### 2.2 Why this direction

- **It is the only mechanism that is correct without a static analysis.** Shared
  memory is shared; there is nothing to keep in sync, so there is nothing that can
  silently fall out of sync (§1.3).
- **It removes mechanisms instead of adding them** — the mirror and the per-call
  copy both go away.
- **It fits the existing architecture.** A payload-only node holds no `Value`s, so
  it cannot participate in a cycle: `Trace` is a no-op and the collector's type
  filter (ADR-0001 §3) keeps paying zero for it. Writes into it use ADR-0013's
  `GcBox` interior mutability, so they have valid provenance. The `Value`
  encoding gains one container tag (ADR-0005) and nothing else changes.
- **It asks for no more than Rakudo guarantees.** MoarVM's `VMArray` reallocates
  too; `pointer-to` there is equally invalid after a resize. Matching that is
  parity, and it keeps us from over-engineering a pinning scheme no dist needs.

---

## 3. Options considered

| Option | Correct without static analysis? | Debt | Verdict |
| --- | --- | --- | --- |
| **A. Native-backed storage + synthesised bodies** (this ADR) | ✓ | **removes** the mirror + the per-call copy | **Adopted** |
| B. Extend the `nativecall_pin` mirror with copy-back | ✗ — no sync point exists (§1.3) | keeps and grows it | Rejected |
| C. Promote to native storage lazily, on first address escape | ✓ if a single store stays authoritative | adds a dual representation | Rejected as the design; permitted as an *allocation* strategy (§5.2) |
| D. Reimplement `NativeHelpers::Blob` natively / shim `BODY_OF` | n/a | private reimplementation | Rejected |
| E. Emulate a real MoarVM object header (`Offset != 0`) | ✓ | invents an internal detail nobody reads | Rejected |
| F. Do nothing — ship `DBIish` SQLite-only | n/a | leaves the mirror in place | Rejected, cost recorded (§4) |

- **B** is the cheap-looking one and the reason this ADR exists: it is wrong in
  the one case the battery needs, and wrong *silently*.
- **C** would let only escaped objects pay, but two representations for `Buf`
  means every one of the ~91 touch sites must handle both — the `locals`↔`env`
  dual-store shape CLAUDE.md names as debt. Note the distinction: allocating the
  native buffer lazily is fine as long as, at any instant, exactly one store is
  authoritative and every access goes through the same accessor.
- **D** contradicts the batteries policy (`docs/batteries/`: adopt community code
  as-is, grow mutsu's core, private reimplementation is a last resort), and does
  not help the 15 dists that use the guts module themselves. A `BODY_OF` shim is
  also not reachable — the sub is defined inside the module being loaded.
- **E** buys nothing: the module's `Offset` probe is deliberately
  implementation-agnostic, and mutsu's answer of 0 already satisfies it.

---

## 4. Consequences

- **`runtime/nativecall_pin.rs` is deleted** at the end of P2, along with its
  `TODO`, and the `CArray` copy-in/copy-out at the end of P3.
- **`.REPR` starts telling the truth for four kinds** and stays `P6opaque` for
  everything else. This is raku parity, and it is also a behaviour change modules
  can branch on — hence the §2.1 ordering rule.
- **`Buf` gets cheaper**: one byte per byte instead of one boxed `Value`, and no
  GC tracing of its contents.
- **A new documented compatibility surface** (`docs/nativecall-repr-bodies.md`)
  describing the three body layouts, the `Offset = 0` contract and the pointer
  validity rule, pinned by a test that reads through a body exactly the way
  `MoarVM::Guts::REPRs` does.
- **Residual unsafety is unchanged in kind, and bounded**: C code writing into a
  live buffer is outside Rust's aliasing model, the same posture ADR-0013 §1.3-2
  records for cross-thread container writes and the same trust `read_field`
  already extends to every declared NativeCall signature. What changes is that it
  becomes *visible and documented* instead of hidden behind a copy that happened
  to paper over it.
- **If rejected or indefinitely deferred**: `DBIish` ships SQLite-only (`01-basic`
  stays at 30/35), `NativeHelpers::Blob`'s 15 dependents stay blocked, the mirror
  and the per-call copy both stay, and `Buf` keeps costing a `Value` per byte.
  That is a coherent position — SQLite is the battery's actual purpose — but it
  should be taken deliberately, not by default.

---

## 5. Open questions — all resolved 2026-07-27

1. **Approve the representation change at all?** ✅ **RESOLVED: yes, P0-P3.**
   tokuhirom: changing mutsu's internals is in scope, and is preferred over a
   compatibility layer that costs performance. Stopping after P1 (option F) is
   off the table: it would leave `pointer-to(Blob)` — the mysql path — permanently
   unsupported and keep the mirror alive.
2. **Where does the native buffer live?** ✅ **RESOLVED: a new payload-only GC
   container kind.** It makes the buffer *be* the object's storage, holds no
   `Value`s (so `Trace` is a no-op and the ADR-0001 type filter keeps paying
   zero), and is reused verbatim by `array[T]` and `CArray[T]`. The alternative —
   an opaque native block hung off the instance's attribute cell — has a smaller
   diff but keeps `Buf` a box-per-byte `Array`, i.e. it is exactly the
   "compatibility layer that costs performance" the governing principle rejects:
   the bytes would exist twice, and every native call would still pay a copy.
3. **Realloc contract:** ✅ **RESOLVED: Rakudo parity — "valid until the container
   is resized or dies".** MoarVM's `VMArray` reallocates too, so pin-on-escape
   would make mutsu *stricter* than the implementation these modules were written
   against, at the cost of holding dead blocks alive. Revisit only if a real dist
   trips on it.
4. **Does `array[T]` ride in P3?** ✅ **RESOLVED: yes.** It is the same node and
   the same marshalling path, and it is independently the fix for the native-typed
   shaped-array defect (`array-shapes.t` T36-38: broken coercion, ~150× slower
   than it should be) — which is the same "boxed where it should be native"
   problem seen from the other side.
5. **How far does `.REPR` truthfulness go?** ✅ **RESOLVED: exactly the four kinds
   that get bodies**, everything else stays `P6opaque`. Answering honestly for
   every type (`P6int`, `P6num`, `P6str`, …) has no known consumer, and under the
   §2.1 ordering rule each honest answer is a promise that a body exists behind
   it — a promise not to make idly.

---

## 6. What starting looks like

P0 is unblocked and self-contained; P1 follows it; P2 is the campaign. Concretely:

- **P0a** `cstruct_layout::write_field` — the mirror of the existing `read_field`,
  wired into the assignment path for a native-handle instance so
  `$handle.field = $v` writes memory instead of being dropped. Pin: extend
  `t/nativecall-*.t` with the calloc round-trip from the finding.
- **P0b** `.^array_type` on `Blob` / `array` / `CArray` metaobjects.
- **P1** a per-object body block (the shape `native_object_where` already
  established) filled as `CStructB` / `CArrayB` from the handle's address, plus
  `.REPR` for those two kinds in the same commit. Acceptance: `LinearArray` from
  `NativeHelpers::CStruct` allocates, indexes and disposes under mutsu.
- **P2** the node, the two accessors, the ~91 `"bytes"` sites, `MVMArrayB`,
  `.REPR = VMArray`, delete `nativecall_pin.rs`. Acceptance: `DBIish` `01-basic`
  reaches raku parity (35/35) and the battery test-suite gate stays green.
- **P3** the same node for `CArray[T]` / `array[T]`; delete the copy-in/copy-out
  in `marshal_carray_arg`. Acceptance: `t/nativecall-carray.t` unchanged and
  green, plus a test that C writes through a retained pointer are visible in Raku
  with no intervening call.

*Status is `Accepted`. If the judgment changes later, supersede this ADR rather
than rewriting it.*
