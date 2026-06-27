use Test;

plan 4;

# Calling a block that references `$_` must not clobber the caller's own `$_`.
# The block gets its own topic (its argument); on return the caller's `$_` is
# restored from its saved frame, NOT overwritten by the callee's stale topic.
# Regression: `$_`/`$/`/`$!` report as dynamic for `.VAR.dynamic`, but they must
# not be propagated back to the caller on return (a `for` loop's topic would
# otherwise revert to the previous iteration's value after each block call).

{
    my $code = { $_ + 100 };
    my @seen;
    for 1..4 {
        my $r = $code.($_);
        @seen.push: $_;          # caller's $_ must still be the loop value
    }
    is-deeply @seen, [1, 2, 3, 4], 'caller $_ survives block.($_) inside a for loop';
}

{
    # The block result itself is correct (it sees its own argument as $_).
    my $code = { $_ * 10 };
    my @out;
    for 1..3 { @out.push: $code.($_); @out.push: $_; }
    is-deeply @out, [10, 1, 20, 2, 30, 3], 'block topic and caller topic both correct';
}

{
    # Smart-match flip-flop with `$_` comparisons relies on the same restore.
    sub test_ff($c, @a) {
        my $ret = '';
        for @a { $ret ~= $c.($_) ?? $_ !! 'x'; }
        $ret;
    }
    is test_ff({ $_ == 3 ff $_ == 5 }, 1..7), 'xx345xx', 'ff over $_ comparisons';
}

{
    # Nested block calls also restore the topic at each level.
    my $inner = { $_ ~ '!' };
    my $outer = { $inner.($_) ~ $_ };
    my @r;
    for <a b c> { @r.push: $outer.($_); @r.push: $_; }
    is-deeply @r, ['a!a', 'a', 'b!b', 'b', 'c!c', 'c'], 'nested block calls keep caller $_';
}
