use Test;

plan 8;

# A closure passed as an argument may be STORED by the callee and invoked long
# after the call returns. Raku closes over containers, so every closure over the
# same `my $c` must see one container -- not a private by-value snapshot.

# --- function argument, sibling closures ---------------------------------
{
    my @handlers;
    sub register(&h) { @handlers.push: &h }

    {
        my $c = 0;
        register { $c = $c + 1 };
        register { $c };
    }

    @handlers[0]();
    is @handlers[1](), 1, 'sibling closure sees a bump made through a function-arg closure';
    @handlers[0]();
    is @handlers[1](), 2, 'and the second bump too';
}

# --- method argument, sibling closures -----------------------------------
{
    class Reg { has @.hs; method register(&h) { @!hs.push: &h } }
    my $r = Reg.new;
    {
        my $c = 0;
        $r.register({ $c = $c + 1 });
        $r.register({ $c });
    }
    $r.hs[0]();
    is $r.hs[1](), 1, 'sibling closure sees a bump made through a method-arg closure';
    $r.hs[0]();
    is $r.hs[1](), 2, 'and the second bump too';
}

# --- the counter shape, called across threads ----------------------------
# A stored handler bumped from a spawned thread must accumulate into the one
# container, not restart from the value captured when it was registered. This
# is the shape of a Cro `route { my $i = 0; get -> { ++$i } }` request counter.
{
    my @handlers;
    sub register(&h) { @handlers.push: &h }

    sub scope(&blk) { blk() }
    scope {
        my $c = 0;
        register { $c = $c + 1 };
    }

    is @handlers[0](), 1, 'first call on the main thread counts';
    is await(start { @handlers[0]() }), 2, 'a call on a spawned thread continues the count';
    is await(start { @handlers[0]() }), 3, 'and so does the next one';
    is @handlers[0](), 4, 'back on the main thread the thread-side bumps are visible';
}
