use Test;

# A `for`-loop pointy-block variable must be a fresh per-iteration binding: a
# closure that captures it sees the value of *its* iteration forever. This
# already worked for a closure created directly in the loop body, but was
# lost the instant the closure was created one closure-*call* deep from the
# loop body (an IIFE factory shape) -- see
# todo/deep/for-loop-var-shared-across-nested-closure-captures.md and
# docs/adr/0027-loop-frozen-value-capture-cascade.md.
#
# This exact shape is what Cro::HTTP::Router's `around` middleware chaining
# uses (RouteSet.transformer), observed as t/http-router.rakutest test 437.

plan 6;

{
    # Baseline: a closure created directly in the loop body already froze
    # correctly (regression pin, not the bug this file targets).
    my @a;
    for (10, 20) -> $v { @a.push: -> { $v } }
    is @a.map({ $_() }).join(","), "10,20",
        'direct closure over pointy param freezes per iteration';
}

{
    # Baseline: an IIFE whose RETURNED closure is stored and called
    # independently (not nested inside a later call) already worked too.
    my @b;
    for (10, 20) -> $v { @b.push: -> $fn { -> { "$v/$fn" } }("X") }
    is @b.map({ $_() }).join(","), "10/X,20/X",
        'IIFE-returned closure stored independently freezes per iteration';
}

{
    # Negative pin: the same nested-closure shape WITHOUT a loop (plain
    # recursive-ish sub calls) must keep working -- the vouch must not fire
    # outside a loop context.
    sub make($v, $fn) { -> $ffn { -> { "$v:{$ffn()}" } }($fn) }
    my $c1 = make(10, -> { "base" });
    my $c2 = make(20, $c1);
    is $c2(), "20:10:base", 'same shape without a loop is unaffected';
}

{
    # The bug repro itself: the IIFE factory chain, matching Cro's `around`
    # middleware pattern (`$callback = -> $fn { -> { $around($fn) } }($callback)`).
    my $callback = -> { "base" };
    for (10, 20) -> $v {
        $callback = -> $fn { -> { "$v:{$fn()}" } }($callback);
    }
    is $callback(), "20:10:base",
        'IIFE factory chain: nested closure keeps the value of its own iteration';
}

{
    # Depth-3 variant: the vouch must cascade through more than one level of
    # nested closure creation (transitivity).
    my $cb = -> { "base" };
    for (1, 2, 3) -> $v {
        $cb = (-> $fn { -> $mid { -> { "$v:{$mid()}" } }(-> { $fn() }) })($cb);
    }
    is $cb(), "3:2:1:base", 'depth-3 nested closure creation cascades the freeze';
}

{
    # A genuinely MUTATED loop-local capture (a live shared cell, not a
    # frozen snapshot) must stay live through the same nested-closure shape
    # -- the gate must not re-freeze a `ContainerRef`.
    my @closures;
    for (10, 20) -> $v {
        my $x = $v;
        my $get = -> { -> { $x } }();
        my $bump = -> { -> { $x += 1 } }();
        @closures.push: ($get, $bump);
    }
    my @seen;
    for @closures -> ($get, $bump) {
        $bump();
        @seen.push: $get();
    }
    is @seen.join(","), "11,21",
        'a mutated (cell-valued) loop-local capture stays live, not frozen';
}

done-testing;
