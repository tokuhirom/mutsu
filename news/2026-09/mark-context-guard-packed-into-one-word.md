# The per-call mark-context guard is one word, not ten boxes

`#7738` measured `bench-fib` under callgrind and found that ~35% of the
benchmark is per-call frame bookkeeping inside
`call_compiled_function_positional_light_at`, spread across a dozen inlined
sites with no dominant one. The single largest of those sites was
`vm_call_state_guard.rs` at 3.50% of the whole benchmark — and essentially all
of it was one guard: `MarkContextGuard`.

## What it was

The "mark context" family is ten one-shot VM flags (`bind_context`,
`scalar_bind_context`, `param_raw_bind_context`, `bound_decont_active`,
`rebind_context`, `constant_context`, `array_share_context`,
`array_share_source`, `explicit_initializer_context`, `vardecl_context`). A
compiler-emitted `Mark*` opcode sets one immediately before a `:=`/vardecl
target's own store op, for that very next store to consume. Because a real
call can sit between the mark and its consumer, every call-dispatch path has
to save, clear and restore the whole family across the boundary
(`MarkContextGuard`) — otherwise a callee's own `my uint8 @state = 0..255`
is treated as a bind target, which is the bug
`t/bind-through-call-boundary-vardecl-leak.t` pins.

Each flag was its own `Box<Cell<bool>>` field on `Interpreter`. The `Box` is
load-bearing: the guard's `Drop` reaches the flag through a raw pointer, and
only a heap allocation *separate* from `Interpreter`'s own survives the
Stacked-Borrows retag that every later `&mut self` call performs over
`Interpreter`'s whole byte range (see `vm_call_state_guard.rs`'s module doc —
two earlier designs were UB and Miri caught both). But *ten* such allocations
meant a guard of ten raw pointers plus ten saved values: ~120 bytes of the hot
call's stack frame, ten `Box` derefs and ten `Cell::get`s to construct, ten
stores to drop — per call, for a family that is all-false at essentially every
call boundary.

## What it is now

`src/runtime/mark_context.rs` packs the nine booleans into one `u16` bitfield
and puts it in the *same* allocation as the one non-`Copy` member
(`array_share_source`), behind a single `Box<MarkContextState>` field. The
`Box` indirection — the whole reason the design exists — is unchanged; there
is simply one of it instead of ten.

The guard is now one raw pointer, one `u16` and one `Option<String>`: save is
a single load, clear a single store, restore a single store.

Call sites read exactly as before. Each flag has an accessor
(`self.vardecl_context()`) handing out a zero-cost `MarkFlag` — a shared borrow
plus a compile-time-constant mask — with the same `get`/`set` API the separate
`Cell<bool>` fields had, so the diff at the ~80 reader sites is one pair of
parentheses.

`vm_run_loop.rs`'s nested-run boundary (EVAL, `dies-ok`/`lives-ok` blocks) did
the same save/clear/restore by hand, nine statements at a time in three
places; it now calls `take_all`/`restore_all`. Folding it closed a gap:
`param_raw_bind_context` was the one family member that boundary never
isolated, though `MarkContextGuard` — the same isolation for an ordinary call
— always did. `t/mark-context-nested-run-isolation.t` pins the boundary,
including that case.

## Measurement

METRICS_PLACEHOLDER

## What this does not fix

`#7738`'s finding is the *shape*, not any one row: the per-call cost is spread
across six `ScopeStack::push_frame`/`pop_frame` pairs, the locals-frame resize,
the env-overlay decision, the pragma save/restore and the routine-frame push,
each individually cheap. This slice takes the largest single row off that list
and leaves the rest; the ticket stays open with its remaining question — whether
the six scope stacks can be folded into one frame object opened and closed once
— unanswered.
