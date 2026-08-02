# A supply block's lexical still leaks to the caller when a thread drives the emit

`news/2026-08/supply-block-lexical-privacy.md` made a `supply { my $x … }`
block's lexicals private to the block: they no longer shadow, and no longer
escape through the `call_sub_value` exit merge. One residual path remains — the
cross-thread lane.

Same-thread tap (correct):

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
mk($s.Supply).tap(-> $v { @g.push($v) });
$s.emit("a"); $s.emit("b"); $s.done;
say $acc;      # raku: OUTER    mutsu: OUTER   OK
```

Emit from a `start` block instead, and the block's `$acc` reappears in the
caller:

```raku
my $done = Promise.new;
mk($s.Supply).tap(-> $v { @g.push($v) }, done => { $done.keep });
start { $s.emit("a"); $s.emit("b"); $s.emit("c"); $s.done }
await Promise.anyof($done, Promise.in(3));
say $acc;      # raku: OUTER    mutsu: abc
```

The emitted values are right in both cases (`a|ab|abc`), so only the writeback
to the caller is wrong. The exit-merge skip added in the fix above does not
cover it: the `whenever` body runs on the spawned thread, and its write to
`$acc` reaches the main thread through the name-keyed `shared_vars` snapshot
lane (`sync_shared_vars_to_env`), not through `call_sub_value`'s `merged` env.

The fix presumably has to teach that lane the same ownership rule — a name the
supply block declared with `my` is not the parent's binding, so a worker's write
to it must not be published back. `SubData::authoritative_captures` on the
`whenever` callback already carries exactly that set (seeded by
`exec_whenever_scope_op`), so the information is available; what is missing is a
consumer on the thread-sync side.

Repro harness: `tmp/sup-accum.p6` (threaded) vs `tmp/sup-accum2.p6`
(same-thread) in the session scratch, or reconstruct from the two snippets
above.
