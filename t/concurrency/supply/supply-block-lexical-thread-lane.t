use Test;

plan 2;

# `news/2026-08/supply-block-lexical-privacy.md` made a `supply { my $x ... }`
# block's lexicals private to the block when the tap runs on the same thread.
# One residual path was suspected to still leak: a worker thread (`start {}`)
# driving the `whenever` body writes its `my $acc` through the cross-thread
# shared_vars snapshot lane instead of `call_sub_value`'s exit merge, which
# the earlier fix did not cover. Verified 2026-08-14: no longer reproduces
# (todo/tickets/supply-block-lexical-leaks-through-thread-lane.md).

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
await Promise.anyof($done, Promise.in(10));

is $acc, "OUTER", 'the caller lexical is untouched when a worker thread drives the emit';
is-deeply @g, ["a", "ab", "abc"], 'the emitted values themselves are still correct';
