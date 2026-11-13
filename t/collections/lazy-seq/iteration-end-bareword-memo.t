use Test;

plan 6;

# The `IterationEnd` bareword is remembered per site; it still answers the
# sentinel a `pull-one` returns, on every thread, and a binding of the name
# still wins.

class Count does Iterator {
    has $.n = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}

sub drain($it) {
    my @got;
    loop {
        my $v := $it.pull-one;
        last if $v =:= IterationEnd;
        @got.push($v);
    }
    @got
}

is-deeply drain(Count.new), [0, 1, 2], 'a pull-one loop stops at the sentinel';
is-deeply drain(Count.new), [0, 1, 2], 'and again from the same site';
is-deeply (await (^4).map({ start { drain(Count.new) } })).List, ([0, 1, 2] xx 4).List,
  'on worker threads';

ok IterationEnd =:= IterationEnd, 'the sentinel is itself';
nok 'IterationEnd' =:= IterationEnd, 'a Str spelling it is not';

sub shadowed() {
    my \IterationEnd = 42;
    IterationEnd
}
is shadowed(), 42, 'a sigilless binding of the name wins';
