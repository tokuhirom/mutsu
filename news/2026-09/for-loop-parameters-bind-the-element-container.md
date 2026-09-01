# A `for` loop parameter binds the element container (ADR-0045, closed)

## Outcome

**Closed 2026-09-01.** This began as a bug report — a closure that escaped a
`for @a -> $v is rw` loop no longer wrote through to `@a` — and grew into
[ADR-0045](../../docs/adr/0045-for-loop-parameters-bind-the-element-container.md),
whose §1.3 measured **27 divergences from raku in five classes**, only one of
which this file had described. All of them are now gone: the ADR's slice 6 sweep
(2026-09-01) re-ran the whole table against `raku` row by row and **all 45 rows
agree**, and `t/for-loop-element-alias.t` pins every one of them, with no
`todo`-marked row left.

The decision was to stop copying the loop variable back into the source at the
end of each iteration, and instead bind the parameter to the element's own
`ContainerRef` (`array_slot_ref` / `hash_slot_ref`) at the bind site — the
primitive [ADR-0036](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md)
had already shipped for `:=`-bound elements. That is what Raku specifies: `for`
binds the *item the iterator yields*, and for a real mutable `Array`/`Hash` that
item **is** the element's `Scalar`.

Three of the five divergence classes needed no closure and no `is rw` at all:
`for @a -> $v { @a[1] = 99 }` silently lost the write, `for @a.reverse -> $v is
rw` wrote each value to the *mirror-image* index, and every mutating loop was
**O(n²)** because each iteration rebuilt the entire backing `ArrayData` to
change one element. The last of those is the headline number: a mutating `<->`
loop over 160 000 elements went from 39.4 s to 0.11 s, and it is flat now rather
than merely faster.

The slices, and where each is written up:

| slice | what | landed |
| --- | --- | --- |
| 0-1 | the pin file; the direct array source with a writable aliasing parameter | 2026-08-27 |
| 2-3 | hash sources; the implicit topic — and the plain named parameter, which turned out to be a *pure deletion* | 2026-08-27 |
| 4 | derived producers (`.values`, `.reverse`, `.sort`, `.kv`), routed at the producer with ADR-0036 slice 3 | 2026-08-27 / 2026-09-01 |
| 5 | bind-time rejection of an `is rw` bind against an immutable source, and the element type constraint | 2026-09-01 |
| 6 | the sweep — and the two shapes it found still on the writeback | 2026-09-01 |

**Slice 6 was not paperwork.** Instrumenting the one function every element
writeback stores through, and running all of `t/` and the whole roast whitelist
under it, found **140 251 stores still happening** — and they were not residue.
An array of `Pair`s had never been promoted at all (`loop_var_unchanged` had no
`Pair` arm, so the identity test failed twice over), which left even a
*read-only* `$sum += .key for @pairs` quadratic at 1.87 s for 8 000 elements;
and the multi-parameter rw loop was reaching raku's write answer **by accident**,
its retained writeback storing the chunk's own cell into the source element after
the fact — so a read through the alias stayed stale, a wholesale rebind was still
clobbered, and that shape stayed O(n²) at 45.7 s for 40 000 elements. Both are
fixed and pinned. ADR-0045 §8 has the detail.

The two claims this file originally made that turned out to be wrong are kept
below, verbatim, because they are the useful part of the record.

**Wrong claim 1 — "the fix is blocked on a share-vs-bind distinction at the
element-store layer."** It is not, and building that distinction would have been
a bug: unconditional write-through **is** the Raku semantics (`@a[0] = "Q"`
assigns *into* the element's `Scalar`, it never replaces it), which ADR-0036 §7
says directly. The hand-written form of the fix already produced raku's answer on
every probe before a line was written. The work was routing, not invention.

**Wrong claim 2 — "this is about deferred closures."** It was one of five
classes, and the three that mattered most to ordinary code involved no closure,
no `rw`, and no concurrency.

The original report follows.

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

## What was still open at the last triage, and where it went

Recorded 2026-09-01, just before slice 6 closed this file:

- row 16 (`.kv` + escaping closure) — landed,
  `news/2026-09/kv-hands-out-element-containers-to-a-multi-param-loop.md`;
- rows 19/30 (`is rw` over a List/Range must die at bind) — landed,
  `news/2026-09/for-loop-is-rw-over-an-immutable-source-fails-at-bind.md`;
- row 28 (typed-array element constraint through the alias) — landed as
  ADR-0036 slice 4,
  `news/2026-09/element-type-check-failures-name-their-container.md`;
- rows 39/39b (`for @$s`) — the promotion was deliberately backed out once and
  re-landed through `ForElementAlias::ArrayValue`; the nqp-type-test fallout it
  caused is its own file.

Two neighbouring gaps stay open and are explicitly **not** this ADR's:

- `for $a, 1, $b, 2 -> \x, $v { x = $v }` writes through to nothing. The source
  is a comma-list of *variables*, not a container, so there is no element to
  promote — `todo/deep/for-loop-pointy-sigilless-param-write-through-missing.md`.
- `Pair.value = X` does not enforce an immutable value. Found during the sweep,
  verified loop-independent —
  `todo/tickets/pair-value-assign-does-not-enforce-immutable-value.md`.
