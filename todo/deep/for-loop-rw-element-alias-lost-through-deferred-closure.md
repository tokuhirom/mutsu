# `for @arr -> $v is rw { ... }` element aliasing doesn't survive into a closure called after the loop

## Status

**Partially fixed (2026-08-27) — ADR-0045 slices 0 and 1 landed.** The headline symptom below is
gone: `for @list -> $v is rw { @callbacks.push(-> { $v = $v + 1 }) }` now binds `$v` to the element's
own `ContainerRef` (`array_slot_ref`) at the bind site, so a closure called after the loop writes
through and the repro prints raku's `[11 21]`.

Slice 1 turned these ADR-0045 §1.3 rows green: **01, 02, 03, 04, 07, 11, 12, 13, 14, 20, 27, 36, 38,
41, 43** — the whole deferred-closure class for a direct array source, the stale-read class for it,
and the class-3 clobber (including row 38, the body rebinding `@a` wholesale). §1.5's O(n²) mutating
`<->` loop is linear now (40 000 elements: 2.13 s → 0.09 s).

**Still open**, and why this file stays here: row 08 (hash sources, slice 2); rows 21, 22, 42, 44 (the
implicit topic and the plain named param, slice 3); rows 16, 17, 24, 39 (derived producers —
`.kv`/`.reverse`/`.sort`/`@$s`, slice 4); rows 19, 28, 30 (bind-time enforcement, slice 5). Each is
`todo`-marked in `t/for-loop-element-alias.t` with its owning slice named. Retire this file to
`news/2026-08/` when ADR-0045's slice 6 lands, per the note below.

The mechanism decision lives in
[ADR-0045](../../docs/adr/0045-for-loop-parameters-bind-the-element-container.md) (2026-08-20):
bind the loop parameter to the element's `ContainerRef` (`array_slot_ref` / `hash_slot_ref`) at the
bind site and retire the per-iteration writeback family. Read ADR-0045 before starting — it carries a
27-row divergence matrix re-measured on `main` (33f75a62f), the invariant table that bounds it, the
writeback-family inventory, the phasing, and the open questions. This file stays open only as the
tracking record; retire it to `news/2026-08/` when ADR-0045's slice 6 lands.

**Two claims below are now known to be wrong, and are kept only for the record:**

1. **The stated blocker does not exist.** This file concluded that a fix must wait on a
   *share-vs-bind distinction at the element-store layer*, because an element store write-throughs
   any `ContainerRef` element unconditionally. [ADR-0036](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md)
   §7 answers this directly: unconditional write-through **is** the Raku semantics (`@a[0] = "Q"`
   assigns *into* the element's `Scalar`, it never replaces it), so the distinction must not be
   built. Re-measured on `main`, the hand-written form of the fix — a `:=`-bound element captured by
   a closure that escapes and is called later — already produces raku's answer on every probe,
   including the `.raku`/`.elems` invisibility invariant. The primitive shipped; the work is routing.
2. **The symptom is much wider than a deferred closure.** The deferred-closure case is one of five
   divergence classes. Three of the others need no closure and no `is rw` at all: an end-of-iteration
   whole-container rebuild **clobbers writes the body made directly to the source**
   (`for @a -> $v { @a[1] = 99 }` loses the write), `for @a.reverse -> $v is rw` writes each value to
   the *mirror-image* index, and the mutating `<->` loop is **O(n²)** (5.2 s for 40 000 elements
   against raku's 0.012 s) because of that same rebuild. See ADR-0045 §1.3 and §1.5.

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
  same session) was a sibling symptom — a `given`/`with`-pointy-param `is rw`
  writeback gap for native types — but it turned out to be an unrelated,
  narrower bug (a compiler detection gap: the native-typed pointy-param
  branch never populated `Given`'s `pointy_param_idx`, so `exec_given_op`
  never knew a pointy param existed at all). Fixed in PR #6334 with a local
  compiler-marker fix; **confirms** this ticket's element-alias gap is a
  genuinely separate, deeper issue, not the same fix in disguise.
- PR #6304 (`given`/`with` pointy-scalar writeback, session 107) fixed a
  *different* instance of "pointy param mutation lost on scope exit" using a
  compile-time-slot-based capture/restore around `BlockLocalScope`'s Nil
  reset — that mechanism does NOT apply here, because the loss here is not a
  scope-exit Nil reset, it is a **snapshot-vs-live-alias representation gap**
  at the array-element level.
- `docs/adr/0027-loop-frozen-value-capture-cascade.md` (loop-var closure
  capture freezing, unrelated bug but adjacent architecture) independently
  arrives at the same "a genuine per-binding `ContainerRef` cell is the
  clean end state" conclusion in its Slice 3 retirement-path note and
  Alternative 4 — for closure-CAPTURE cell identity there, for array-ELEMENT
  aliasing here. Different surface, possibly a shared underlying primitive;
  read both before scoping a "fresh cell per binding" campaign.

## Suggested next steps (superseded by ADR-0045 §4 — kept for the record)

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
