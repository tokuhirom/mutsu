# Element assignment through an `is rw` parameter holding a Buf replaces the whole Buf with a fresh Array

## Affected tests

- `t/http2-frame-serializer.rakutest` subtest 11 ("Simple priority frame is not parsed back!"): serializing the Priority frame works (subtest 10 passes), but parsing it back dies inside `Cro::HTTP2::FrameParser`'s `my multi sub payload(2, Buf $data is rw, ...)` (FrameParser.rakumod line 159-166) at line 162 `$data[0] +&= 0x7F;` with:

  ```
  Type check failed for an element of $data; expected Buf but got Int (0)
  ```

  The parser's whenever body dies, the test's parser tap never fires, `$complete` times out, `flunk "$desc is not parsed back!"` fires.

  (The Header-scan `$data[5] +&= 0x7F` at FrameParser line 55 does NOT hit this because there `$data` is a plain `my` local, not a typed rw parameter.)

## Repro

```raku
sub f($d is rw) { $d[0] = 3; }
my $b = Buf.new(0x80, 1);
f($b);
say $b.raku;
```

- mutsu (release and debug): `[3]` — the caller's Buf is replaced by a plain Array containing only the assigned element.
- raku: `Buf.new(3,1)`.

Variants (all verified):

| Signature | mutsu result for `$d[0] = 3` on `Buf.new(0x80,1)` |
|---|---|
| `sub f(Buf $d)` (no rw) | `Buf.new(3,1)` — correct |
| `sub f($d is rw)` | `[3]` — Buf lost |
| `sub f(Buf $d is rw)` | dies `Type check failed ... expected Buf but got Int` (the Cro shape) |
| `sub f($d is rw) { $d[1] = 9 }` | `[Any, 9]` — autoviv into a fresh Array |
| `sub f(Array $d is rw)` on `[9,1]` | `$[3, 1]` — correct |

Compound ops (`+&=`) go through the same read-modify-write and fail identically.

## Root cause

Reads through the rw parameter work (`$d[0]` returns 128), so the read path derefs the rw binding to the Buf. The **write** path does not: index assignment compiles to `Expr::IndexAssign` → `OpCode::IndexAssignExprNamed` → `exec_index_assign_expr_named_op` (`src/vm/vm_var_assign_element.rs:409` → `src/vm/vm_var_assign_index_named.rs:249`). That path has a dedicated Buf/Blob element-assignment lane (`vm_var_assign_index_named.rs` ~line 848: "Buf/Blob element and slice assignment") — but it is only reached when the variable's current value is seen as a Buf. When the callee's `$d` is an `is rw` binding (rw proxy / VarRef, not the raw Buf), the target classification misses the Buf lane and falls into the array autoviv lane, which builds a **fresh Array** sized to the index (hence `[3]` / `[Any, 9]`), and the rw writeback then propagates that Array (or, with a `Buf` type constraint on the parameter, fails the writeback type check with "expected Buf but got Int" — note the element value, not the array, reaches the check).

The exact branch to fix is where `exec_index_assign_expr_named_op_inner` resolves the assignment target: it must deref an rw/VarRef target to its underlying value *before* classifying Buf vs Array vs autoviv, and route Buf/Blob targets into the existing Buf element lane so the mutation happens in the shared Buf cell (same as the non-rw parameter path already does).

## Fix direction

- In `src/vm/vm_var_assign_index_named.rs` (`exec_index_assign_expr_named_op_inner`, line 249ff): before the autoviv/array classification, if the target var's value is a VarRef/rw proxy, resolve it and re-dispatch on the underlying value; ensure the Buf/Blob lane (~line 848) accepts that resolved target and writes through to the shared cell, followed by the normal rw writeback of the (still-Buf) value.
- Audit the sibling ops for the same gap: `IndexElemAutoviv`, `PostIncrementIndex`/`PostDecrementIndex` (compound `+&=` uses read-modify-write through the same named-index ops), and the slice-assign variant.
- Risk: rw writeback of container values is delicate (see `news/2026-08` rw-accessor cell work); add pins for both the typed (`Buf $d is rw`) and untyped (`$d is rw`) shapes, and for compound ops.

## Verification

- New pin test `t/rw-param-buf-element-assign.t`: the repro table above (plain assign, compound `+&=`, index beyond 0, typed and untyped rw params), asserting the caller's Buf after the call.
- `t/http2-frame-serializer.rakutest` subtest 11 passes (file needs the UInt-enum ticket `http2-uint-enum-typecheck.md` to reach its plan, and the lexical-sub ticket for subtests 4/5).
- `tmp/h2-priority-parse.raku` (croflake.sh) prints the parsed Priority frame instead of QUIT.
