use Test;

# The Iterator protocol on a built-in iterator, called directly and through a
# user iterator that delegates to one (#17337): every answer is the one the
# protocol gives, call after call at the same site.

plan 9;

{
    my $it := (1, 2, 3).iterator;
    my @got;
    loop {
        my $v := $it.pull-one;
        last if $v =:= IterationEnd;
        @got.push: $v;
    }
    is-deeply @got, [1, 2, 3], 'pull-one walks a list iterator to IterationEnd';
    ok $it.pull-one =:= IterationEnd, 'an exhausted iterator stays exhausted';
}

{
    my $it := (^10).iterator;
    $it.skip-one for ^3;
    is $it.pull-one, 3, 'skip-one advances the same iterator pull-one reads';
    is $it.count-only, 6, 'count-only counts what is left';
    ok $it.bool-only, 'bool-only answers while values remain';
}

{
    my $it := (1, 2).map(* * 10).iterator;
    is-deeply ($it.pull-one, $it.pull-one), (10, 20), 'a map iterator pulls one value per call';
    ok $it.pull-one =:= IterationEnd, 'and then IterationEnd';
}

class Delegating does Iterator {
    has $.source;
    has int $.pulled;
    method pull-one() {
        my $v := $!source.pull-one;
        $!pulled++ unless $v =:= IterationEnd;
        $v
    }
}

{
    my $d = Delegating.new(source => (^1000).iterator);
    my $seq := Seq.new($d);
    is $seq.max, 999, 'a user iterator over a built-in one feeds Seq.max';
    is $d.pulled, 1000, 'every element was pulled once';
}
