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
