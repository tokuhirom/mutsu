use Test;

plan 12;

# `.map`/`.grep` on a Seq over a lazy iterator reified the whole iterator
# first, so an infinite one hung (#10891). The stage now pulls from the
# iterator on demand.

is-deeply Seq.new((1..*).iterator).map(* + 1).head(2).List, (2, 3),
    'map over a lazy built-in iterator Seq';
is-deeply Seq.new((1..*).iterator).grep(* % 2).head(2).List, (1, 3),
    'grep over a lazy built-in iterator Seq';
is-deeply Seq.from-loop({ 1 }).map(* + 1).head(2).List, (2, 2),
    'map over an infinite Seq.from-loop';

class Ones does Iterator {
    method pull-one { 1 }
    method is-lazy { True }
}
is-deeply Seq.new(Ones.new).map(* + 1).head(2).List, (2, 2),
    'map over a lazy user iterator Seq';

{
    my $n = 0;
    my $m = Seq.from-loop({ $n++ }).map(* * 2);
    is-deeply $m[0..2].List, (0, 2, 4), 'subscripting the stage pulls only what it needs';
    ok $m.is-lazy, 'the stage is lazy because its source is';
    is $m.gist, '(...)', 'and gists as (...)';
}

throws-like { Seq.new((1..*).iterator).map(* + 1).elems }, X::Cannot::Lazy,
    'elems on the infinite stage throws instead of hanging';

{
    my $s = Seq.new((1..*).iterator);
    is-deeply $s.map(* + 1).head(3).List, (2, 3, 4), 'map consumes the Seq';
    throws-like { $s.map(* + 1) }, X::Seq::Consumed, 'a second map on it throws';
}

# A finite iterator Seq is not lazy and maps as before.
{
    my $s = Seq.new((1, 2, 3).iterator);
    is-deeply $s.map(* + 1).List, (2, 3, 4), 'map over a finite iterator Seq';
    nok Seq.new((1, 2, 3).iterator).map(* + 1).is-lazy, 'and the result is not lazy';
}
