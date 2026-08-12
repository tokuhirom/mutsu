# `for @arr -> $v is rw { ... }` element aliasing doesn't survive into a closure called after the loop

## Symptom

```raku
my @list = 10, 20;
my @callbacks;
for @list -> $v is rw {
    @callbacks.push(-> { $v = $v + 1; $v });
}
say @callbacks[0]();
say @callbacks[1]();
say @list;
```

raku: `11`, `21`, `[11 21]` — the closure's `$v` still aliases the array
element even when called after the loop has finished, so the mutation
writes through.

mutsu: `11`, `21`, `[10 20]` — the callback's return values are correct (so
the closure's OWN view of `$v` is right), but the mutation never reaches
`@list`. Confirmed pre-existing via a worktree build at `af1d75401` (the
commit immediately before ADR-0027 Slice 1 / PR #6309 merged) — same broken
output there, so this is unrelated to the ADR-0027 loop-freeze-cascade
mechanism (`owned_captures`/`frame_owned`) and its Slice 1 fix.

## Root cause (confirmed 2026-08-12)

There is no live `ContainerRef` aliasing at all for a `for @arr -> $v is rw
{...}` loop param — `src/vm/vm_for_loop_body.rs:527` binds `$v` into
`env`/`locals` as a **plain value clone** (`item.clone()`) each iteration.
Body mutations just reassign that plain local like any other variable, and
`write_back_for_rw_param` (called once, right after the body finishes
normally — the `Ok(())`/`is_succeed()` arms around
`src/vm/vm_for_loop_body.rs:660` and `:711`) reads `$v`'s value **at that one
instant** and copies it into `@list[idx]`. It is a **snapshot writeback**,
not a persistent alias — there is exactly one writeback point per iteration,
and it happens synchronously right after the loop body statement finishes.

This exactly explains the observed split:

- **In-body direct mutation** (`for @list -> $v is rw { $v = $v + 1 }`) and
  **immediate closure calls within the same iteration** (even via an
  escaped-looking `@callbacks.push(...); @callbacks[*-1]()` called
  *before* the iteration ends) both mutate `$v`'s plain local **before**
  `write_back_for_rw_param` runs, so the snapshot it takes is already
  correct. Confirmed both cases work (see repro2/repro3 below).
- **A closure stored and called after the loop** mutates `$v`'s captured
  cell (created by the general closure-capture mechanism, e.g.
  `box_captured_lexicals`, which boxes the plain value into its own
  `ContainerRef` at capture time) — but that box is a **separate, disconnected
  cell** from `@list[idx]`, because the writeback already copied a plain
  value into the array before the closure ever gets a chance to run. Once the
  snapshot has been taken, nothing keeps the array element and the closure's
  cell in sync.

This is the **same underlying architecture gap** as the historical
"scalar-array-sharing" campaign's blocked **Slice 2b** (see project memory
`scalar-array-sharing-2c-and-2b-blocker`, 2026-06-18): a genuine per-element
`ContainerRef` alias for an *array element itself* is not supported, because
the single-element array store write-through
(`exec_index_assign_expr_named_op_inner` in `vm_var_assign_ops.rs`, roughly
line 3836 as of that memory — re-verify the line before relying on it)
write-throughs **any** `ContainerRef` element unconditionally on reassign,
with no way to distinguish "this element is a bind-target that should stay
aliased" (what a for-loop `is rw` param + escaping closure needs) from "this
element was just plain-reassigned and should replace whatever was there"
(ordinary `@arr[i] = newval`). A correct fix needs that share-vs-bind
distinction at the element-store layer before a for-loop `is rw` param can
bind a *genuine, closure-surviving* alias to its source element instead of
doing periodic snapshot writeback.

**Do NOT attempt a narrow patch inside `vm_for_loop_body.rs` alone** — the
snapshot-writeback mechanism there is not "slightly wrong," it is
structurally the wrong shape for this case (no way to distinguish "the array
element must now be the SAME cell a live closure holds" without the
element-store layer support above). This is `todo/deep` scope, not a small
ticket — see "Where this connects" below before starting design.

## Where this connects

- `todo/deep/element-itemization-lost-in-scalar-binding.md` is a *different*
  (also store-side, but read-representation, not aliasing) gap — do not
  conflate the two; that ticket's "ADR-0013 §7 unblocked this" note likely
  also applies here (same `GcBox`/`UnsafeCell` interior-mutability layer),
  worth checking when scoping a fix design.
- `todo/tickets/native-pointy-param-is-rw-writeback-missing.md` (filed the
  same session) is a sibling symptom — a `given`/`with`-pointy-param `is rw`
  writeback gap for native types — but per that ticket's own repro it may be
  a narrower, native-type-specific issue rather than this same element-alias
  gap; verify independently before assuming a shared fix.
- PR #6304 (`given`/`with` pointy-scalar writeback, session 107) fixed a
  *different* instance of "pointy param mutation lost on scope exit" using a
  compile-time-slot-based capture/restore around `BlockLocalScope`'s Nil
  reset — that mechanism does NOT apply here, because the loss here is not a
  scope-exit Nil reset, it is a **snapshot-vs-live-alias representation gap**
  at the array-element level.

## Suggested next steps

1. Given the confirmed root cause is element-level `ContainerRef` aliasing
   (Slice 2b of the old scalar-array-sharing campaign), this warrants a
   proper design pass (Fable) before implementation: define the share-vs-bind
   distinction for array-element `ContainerRef`s (e.g. a dedicated marker
   variant or an explicit "this index is an active bind target" side-table)
   so `@arr[i] = plain_value` keeps today's replace semantics while a
   for-loop `is rw` param (and any other element-bind consumer) can install a
   real alias.
2. Once designed, `write_back_for_rw_param` becomes largely unnecessary for
   the closure-capture case: if `$v`'s per-iteration binding is itself the
   live element alias (not a plain value), body mutation, immediate-call
   mutation, AND deferred-closure mutation all write through the same cell
   automatically, with no separate snapshot step needed at iteration end.
3. Verify the fix against the existing `write_back_for_rw_param` call sites
   for QuantHash/multi-param cases too (`write_back_quanthash_rw`,
   `kv_mode`) — those may need the same aliasing treatment or may be fine to
   leave on the snapshot path if they don't have an analogous closure-escape
   gap (untested; check before assuming either way).

## Reproduce

The repro above, no fixtures needed. Expected (raku): `11`, `21`,
`[11 21]`. Actual (mutsu): `11`, `21`, `[10 20]`.
