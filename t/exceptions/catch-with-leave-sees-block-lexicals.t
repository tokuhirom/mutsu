use Test;

# A block with both a CATCH and a LEAVE runs the CATCH while the block is
# still live -- its lexicals bound -- and the LEAVE afterwards. EEL's CLI
# (`CATCH { default { note .message; return $phase } }` in a routine with
# LEAVE phasers) returned Nil instead of its exit code. Identical under rakudo.
plan 6;

my @order;
sub from-closure(--> Int) {
    my $phase = 2;
    CATCH { default { @order.push: "catch $phase"; return $phase } }
    my @p = (1,).map(-> $n { die "x" });
    my $tap;
    LEAVE { @order.push: 'leave' }
    0
}
is from-closure(), 2, 'CATCH reads a lexical of the block when a closure died';
is-deeply @order, ['catch 2', 'leave'], 'CATCH runs before LEAVE';

sub thrower { die "deep" }
sub from-call(--> Int) {
    my $phase = 3;
    CATCH { default { return $phase } }
    thrower();
    LEAVE { }
    0
}
is from-call(), 3, 'the same when a called routine died';

sub value-tail {
    CATCH { default { } }
    42;
    LEAVE { }
}
is value-tail(), Nil, 'a trailing LEAVE still makes the value Nil';

sub no-throw {
    my $x = 7;
    CATCH { default { } }
    LEAVE { }
    $x
}
is no-throw(), 7, 'without an exception the tail value comes through';

my $seen;
{
    my $y = 'block';
    CATCH { default { $seen = $y } }
    LEAVE { }
    (1,).map({ die 'z' }).eager;
}
is $seen, 'block', 'a bare block with CATCH and LEAVE';
