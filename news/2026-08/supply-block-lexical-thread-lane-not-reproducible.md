# A supply block's cross-thread lexical leak no longer reproduces

`news/2026-08/supply-block-lexical-privacy.md` made a `supply { my $x … }`
block's lexicals private to the block on the same-thread tap path. A residual
cross-thread path was suspected to still leak a worker thread's write to the
block's `my $acc` back into the caller's own same-named lexical, since the
`whenever` body's write reaches the main thread through the name-keyed
`shared_vars` snapshot lane rather than `call_sub_value`'s exit merge:

```raku
sub mk($in) {
    supply {
        my $acc = "";
        whenever $in -> $x { $acc ~= $x; emit $acc }
    }
}
my $acc = "OUTER";
my $s = Supplier.new;
my @g;
my $done = Promise.new;
mk($s.Supply).tap(-> $v { @g.push($v) }, done => { $done.keep });
start { $s.emit("a"); $s.emit("b"); $s.emit("c"); $s.done }
await Promise.anyof($done, Promise.in(3));
say $acc;      # raku: OUTER
```

Verified 2026-08-14: no longer reproduces. `$acc` prints `OUTER`, matching
`raku`, stable across five repeated runs. Fixed as a side effect of unrelated
interpreter work in the twelve days since this ticket was filed (likely one of
the many cross-thread `shared_vars`/env-sync fixes landed in that window — not
re-bisected, since the behavior is now correct and general). Pinned by the new
`t/supply-block-lexical-thread-lane.t`.
