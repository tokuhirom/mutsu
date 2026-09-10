use Test;

plan 3;

# A `supply { … }` block's emitter has a name generated per PARSE SITE, but a
# routine that returns such a block can be called more than once, so one parse
# site can have several live instances. Tapping the outer instance runs the
# inner one's body nested inside it; the `whenever` closures each instance
# creates must keep answering their OWN emitter. When they did not, the outer
# body's `emit` went back into the inner supply and fed itself forever — which
# is exactly what a Cro middleware pipeline (`$s = wrap($s)` repeated) builds.

sub xform(Supply $in --> Supply) {
    supply whenever $in -> $v { emit "w($v)" }
}

{
    my $s = supply emit 'REQ';
    $s = xform($s);
    $s = xform($s);
    my @got;
    $s.tap(-> $v { @got.push($v) });
    is @got, ['w(w(REQ))'], 'two instances of one supply block chain, not loop';
}

{
    # Three deep, to make sure the fix is not "the second one happens to work".
    my $s = supply emit 1;
    $s = xform($s) for ^3;
    my @got;
    $s.tap(-> $v { @got.push($v) });
    is @got, ['w(w(w(1)))'], 'three instances of one supply block chain';
}

{
    # Several values through a two-instance chain: each must be wrapped exactly
    # twice and arrive in order.
    my $s = supply for 1..3 { emit $_ }
    $s = xform($s);
    $s = xform($s);
    my @got;
    $s.tap(-> $v { @got.push($v) });
    is @got, ['w(w(1))', 'w(w(2))', 'w(w(3))'], 'every value crosses the chain once';
}
