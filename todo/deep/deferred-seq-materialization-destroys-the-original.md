# Materializing a deferred `Seq` destroys the value it was asked about

A second method call on the same `Seq` throws, where raku answers:

```
$ cat tmp/lz.raku
my $p = "tmp/lz.txt".IO;
$p.spurt("A\nB\nC\n");
my $g = $p.open(:r).lines;
say "Str 1: '" ~ $g.Str ~ "'";
say "Str 2: '" ~ $g.Str ~ "'";

$ raku tmp/lz.raku
Str 1: 'A B C'
Str 2: 'A B C'

$ mutsu tmp/lz.raku
Str 1: 'A B C'
The iterator of this Seq is already in use/consumed by another Seq ...
```

Merely *asking about* the value is enough — `.defined` alone destroys it:

```
my $g = $p.open(:r).lines;
say $g.defined;      # True
say $g.Str;          # raku: 'A B C'   mutsu: X::Seq::Consumed
```

## Where

`src/runtime/methods_call_dispatch.rs`, the deferred-iterator arm. `IO::Handle.lines`
(and every other `Seq.new(<Iterator>)` producer — see
`news/2026-08/…deferred-seq-reification…`) stores the iterator without pulling.
When a method arrives, the arm pulls every item into a **new** `Arc`, builds a
**new** `Seq`, and re-dispatches the method on that:

```rust
if !matches!(method, "cache" | "sink" | "raku" | "perl") {
    let items_arc = items.clone();
    if let Some(iterator) = crate::value::seq_take_deferred_iter(&items_arc) {
        ... pull every item into `pulled_items` ...
        let new_seq = Value::seq_arc(std::sync::Arc::new(pulled_items));
        // cached state is transferred; the ITEMS are not
        return self.call_method_with_values(new_seq, method, args);
    }
}
```

`seq_take_deferred_iter` removes the iterator from the side table, and the pulled
items land in a different `Arc`. The variable the user still holds therefore ends
up with an empty item list and no iterator — indistinguishable from a consumed
`Seq`, which is exactly what the next call reports.

## Why raku does not have this problem

rakudo's `Seq.Str` (and `.gist`, `.raku`, …) go through `self.cache`, which
reifies **into the Seq itself**. The iterator is consumed once; the Seq keeps the
values, so every later access — of any kind — reads them back. Only *iterating*
a Seq twice is an error there, and `.Str` is not iterating.

## Why it matters now

rakudo's real `Test.rakumod` opens `is` with

```raku
multi sub is(Mu $got, Mu:D $expected, $desc = '') is export {
    if $got.defined {                 # <-- destroys a deferred Seq here
        my $test = ... $got eq $expected;
```

so `is $fh.lines, <A B C>` compares an already-gutted Seq and reports
`got: '(...)'`. It is one of the remaining gaps in
`todo/tickets/vendor-real-test-module.md` (`t/is-lazy-io-lines.t`), and any
`t/` file that hands a lazy `.lines` to the real module will hit it.

## The fix, and why it is not a one-liner

The sound fix is to reify **in place**: write the pulled items back through the
original `Arc` so every alias of the value sees them, and mark the Seq cached,
rather than building a fresh `Seq` and abandoning the old one. That is the same
shape as the `arc_contents_mut` / `gc_contents_mut` chokepoint work, and it wants
that machinery rather than a local hack.

Two narrower options were tried and rejected:

- **Exempting `.defined`/`.DEFINITE` from the materialize path** (their answer
  cannot depend on the contents, so it is a correct restriction on its own) does
  *not* fix the ticket: the very next call, `$got eq $expected`, materializes and
  the diagnostic still renders `(...)`. It also had no observable effect on the
  `.defined`-then-`.Str` repro above, which suggests `.defined` on a deferred Seq
  reaches the reification through a different path — worth locating before
  touching this arm at all.
- **Transferring the pulled items to the old `Arc` after the fact** is the same
  in-place write, just spelled less honestly; do the real thing.

Pin candidates: the two-call repro above, `.defined`-then-`.Str`, and
`t/is-lazy-io-lines.t` under `MUTSU_REAL_TEST=1`.

## Residual try-cell divergences (2026-08-14)

Split out of `todo/tickets/sinking-a-try-blocks-discarded-value-escapes-the-try.md`
(now closed — its own motivating symptom was fixed by #6115, and its
`try`-statement sink placement was verified rakudo-conformant; see
`news/2026-08/try-statement-sink-semantics-pinned.md`). That investigation's
probe matrix found a family of cells where mutsu is *more forgiving* than raku
— it never aborts a file raku would pass, only passes constructs raku would
abort — all tracing to the SAME eager-Seq-reification root cause this ticket
already owns (mutsu forces a `map` Seq at the call/assignment boundary; raku
keeps it lazy until something actually consumes it). Each snippet is `raku` /
`mutsu`:

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

These will align toward raku automatically once sub-returned/assigned Seqs
stay lazy (the fix this ticket already tracks) — **landing that laziness fix
makes mutsu STRICTER in every one of these cells** (constructs that pass today
will start aborting, matching raku), so a full roast-whitelist sweep is
mandatory when it lands, not just `make test`. `t/try-sink-semantics.t`
(added alongside the now-closed ticket) pins the cells that already match and
must keep matching; re-run it after any laziness change here.
