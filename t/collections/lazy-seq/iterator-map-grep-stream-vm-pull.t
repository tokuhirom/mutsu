use Test;

# The VM serves a `.map`/`.grep` stream iterator's `pull-one` and skip family
# straight from its native iterator dispatch, through the same protocol step
# every other path runs (#10217). These pin the behaviour that path must keep.

plan 22;

{
    my $c = 0;
    my $it = (1..10).map({ $c++; $_ * 2 }).iterator;
    is $it.skip-at-least(3), 1, 'skip-at-least over a stream';
    is $c, 3, '... runs the callback for the skipped elements only';
    is $it.skip-at-least-pull-one(2), 12, 'skip-at-least-pull-one';
    is $c, 6, '... runs the callback up to the element it hands out';
    is $it.skip-at-least(100), 0, 'skip-at-least past the end returns 0';
    ok $it.pull-one =:= IterationEnd, '... and leaves the stream exhausted';
}

{
    my $c = 0;
    my $it = (1..5).map({ $c++; $_ }).iterator;
    $it.pull-one;
    ok $it.sink-all =:= IterationEnd, 'sink-all returns IterationEnd';
    is $c, 5, '... after running every remaining callback';
    ok $it.pull-one =:= IterationEnd, 'pull-one after sink-all';
}

{
    my $it = (1..4).map(* + 100).iterator;
    my $alias = $it;
    is $it.pull-one, 101, 'pull-one through one variable';
    is $alias.pull-one, 102, '... the alias continues from the advance';
    is $it.pull-one, 103, '... and so does the original';
}

{
    my @target;
    my $it = (1..6).map(* * 3).iterator;
    $it.push-exactly(@target, 1);
    is $it.skip-one, 1, 'skip-one after a push';
    is ($it.pull-one, $it.pull-one), (9, 12), 'pull-one continues after the skip';
}

{
    my $c = 0;
    my $it = (1..*).map({ $c++; $_ ** 2 }).iterator;
    is ($it.pull-one, $it.pull-one, $it.pull-one), (1, 4, 9),
        'pull-one over an infinite stream';
    is $c, 3, '... runs one callback per pull';
}

{
    my $it = (1..3).map({ die "boom $_" if $_ == 2; $_ }).iterator;
    is $it.pull-one, 1, 'the element before a dying callback';
    throws-like { $it.pull-one }, Exception, message => 'boom 2',
        'an exception in the callback propagates out of pull-one';
}

{
    my @odd;
    my $it = (1..9).grep(* % 2).iterator;
    until (my $v := $it.pull-one) =:= IterationEnd { @odd.push: $v }
    is @odd, [1, 3, 5, 7, 9], 'draining a grep stream with pull-one';
}

{
    my $it = (1..3).map(* + 1).iterator;
    is $it.skip-one, 1, 'skip-one on a fresh stream';
    is $it.pull-one, 3, '... pull-one yields the element after it';
    my @rest;
    $it.push-all(@rest);
    is @rest, [4], 'push-all after pull-one takes the rest';
}
