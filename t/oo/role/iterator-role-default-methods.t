use Test;

plan 20;

# A class that `does Iterator` and writes only `pull-one` gets the role's
# default protocol methods, built on that `pull-one` as rakudo's
# `Iterator.rakumod` builds them. (#9466)

class I does Iterator {
    has $.n = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}

my @a;
is I.new.push-exactly(@a, 2), 2, 'push-exactly answers the count';
is-deeply @a, [0, 1], '... and pushed that many';
my @b;
ok I.new.push-exactly(@b, 5) =:= IterationEnd, 'push-exactly past the end is IterationEnd';
is-deeply @b, [0, 1, 2], '... having pushed what there was';
my @c;
ok I.new.push-all(@c) =:= IterationEnd, 'push-all answers IterationEnd';
is-deeply @c, [0, 1, 2], '... and pushed everything';
my @d;
I.new.push-until-lazy(@d);
is-deeply @d, [0, 1, 2], 'push-until-lazy pushes a non-lazy iterator whole';
my $buf = IterationBuffer.new;
I.new.push-at-least($buf, 2);
is-deeply $buf.List, (0, 1), 'push-at-least into an IterationBuffer';
ok I.new.sink-all =:= IterationEnd, 'sink-all answers IterationEnd';
is I.new.skip-one, 1, 'skip-one answers 1 when it skipped';
is I.new.skip-at-least(2), 1, 'skip-at-least within range';
is I.new.skip-at-least(5), 0, 'skip-at-least past the end';
is I.new.skip-at-least-pull-one(2), 2, 'skip-at-least-pull-one';
is I.new.is-lazy, False, 'is-lazy defaults to False';
is I.new.is-deterministic, True, 'is-deterministic defaults to True';
is I.new.is-monotonically-increasing, False, 'is-monotonically-increasing defaults to False';
ok I.new.can('push-all'), '.can sees the defaults';

class J does Iterator {
    method pull-one { 1 }
    method is-lazy { True }
    method skip-one { 'mine' }
}
my @e;
is J.new.push-until-lazy(@e), True, 'push-until-lazy stops at a lazy iterator';
is J.new.skip-one, 'mine', "the class's own method wins over the default";

is-deeply Seq.new(I.new).List, (0, 1, 2), 'Seq.new over the iterator';
