# Residual try-cell divergences from eager sub-returned/assigned Seq reification

Split out of `todo/deep/deferred-seq-materialization-destroys-the-original.md`, which is now
retired (its headline symptom — a second method call on a deferred `Seq` destroying the value —
was fixed by [ADR-0034](../../docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md),
see `news/2026-08/seq-reification-distinct-from-consumption.md`). ADR-0034 §6 explicitly scopes
this part out as a **different** defect: mutsu forces a `map`-produced `LazyList` at the
assignment/call boundary, where raku keeps it lazy until something actually consumes it — that is
about **where** forcing happens (in `LazyList`), not about **what** forcing does to a `Seq` (which
ADR-0034 fixed). Landing ADR-0034 removed the coupling that made these cells hard to fix (every
extra touch used to be a chance to hit the destroy-on-materialize bug), so this work is now
tractable on its own terms, but it has not been attempted yet.

Originally found while investigating `todo/tickets/sinking-a-try-blocks-discarded-value-escapes-the-try.md`
(now closed — its own motivating symptom was fixed by #6115, and its `try`-statement sink
placement was verified rakudo-conformant; see `news/2026-08/try-statement-sink-semantics-pinned.md`).
That investigation's probe matrix found a family of cells where mutsu is *more forgiving* than
raku — it never aborts a file raku would pass, only passes constructs raku would abort. Each
snippet is `raku` / `mutsu`:

- P4: `try { (1..3).map({die "boom"}) }; say "alive ", $!.defined` (unit scope)
  — raku: **throws**; mutsu: `alive True` (caught inside the try, since the
  force happens before the try's own boundary closes).
- P5: `sub f { (1..3).map({die "boom"}) }; try { f() }; say ...` — same shape,
  one level of call indirection; same divergence.
- P12/P13: `sub f { (1..3).map({die "boom"}) }; sub ee { try { f() }; $! };
  say ee().^name` (literal-`.map`-tail variant too) — raku: **throws**; mutsu:
  `X::AdHoc` + alive (caught).
- P18: `sub ee { try { f() } }; say ee().^name` (die-Seq, try final, value
  used) — raku: `Seq` + alive, never forced; mutsu: `Nil` + alive, forced and
  caught.
- Q5/Q6: yada-stub `map -> $x,$y { ... }, 1..6` reached via `try { map ... }`
  or `try { f() }` inside a sub — raku: **throws** (`in sub ee`); mutsu:
  `Failure` + alive.
- Q9: `try { (1..3).map({die "boom"}) }; CATCH { default { say
  "unit-caught" } };` at unit scope — raku: **unit-caught** (the escape is
  caught by the *enclosing* block's CATCH, since raku's try has already let it
  through); mutsu: alive, no unit-caught (mutsu's inner try already caught it
  before it could escape).
- Q11: `try { EVAL $c }` in a sub, `$c` = die-Seq code — raku: **throws**;
  mutsu: `X::AdHoc` + alive.
- Q14: `sub f { (1..3).map({ fail "x" }) }; try { f() }; say ...` (unit) —
  raku: **throws**; mutsu: `alive True`.
- R6/R7: Q5/Q6 with a `reached-tail` marker after the `try` — raku: throws
  before the marker prints (R6) or throws after invoking the block (R7);
  mutsu: `reached-tail` prints, `r=Failure`/`r=X::StubCode`, alive — the
  marker printing at all is the tell that mutsu's force happened too early.

These will align toward raku automatically once sub-returned/assigned Seqs stay lazy — **landing
that laziness fix makes mutsu STRICTER in every one of these cells** (constructs that pass today
will start aborting, matching raku), so a full roast-whitelist sweep is mandatory when it lands,
not just `make test`. `t/try-sink-semantics.t` pins the cells that already match and must keep
matching; re-run it after any laziness change here.

## Where to start

The root cause is `LazyList` forcing at the sub-return/assignment boundary rather than at first
consumption — a different mechanism from `Seq`'s `SeqBody` (ADR-0034 §6 confirms `LazyList` is
explicitly out of scope there). Look at `force_lazy_list_vm`/`force_lazy_list_vm_n`'s callers in
`src/vm/vm_helpers_lazy.rs` and wherever a sub return value or `=`-assignment RHS gets eagerly
forced before it reaches its actual first consumer.
