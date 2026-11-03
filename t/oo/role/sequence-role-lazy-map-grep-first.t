use v6;
use Test;

# `map`, `grep` and `first(matcher)` of a `does Sequence` class are lazy:
# an endless `iterator` answers once a following `head(n)` has what it needs
# (#16133).

plan 14;

class Endless does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n++ }
}
class Sq does Sequence { method iterator { Endless.new } }

is-deeply Sq.new.map(* * 2).head(3).List, (0, 2, 4), 'map then head(3)';
is-deeply Sq.new.grep(* %% 2).head(2).List, (0, 2), 'grep then head(2)';
is Sq.new.first(* > 5), 6, 'first(matcher)';
is Sq.new.first(* %% 7), 0, 'first(matcher) matching the first element';

class Finite does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n < 4 ?? $!n++ !! IterationEnd }
}
class FSq does Sequence { method iterator { Finite.new } }

is-deeply FSq.new.map(* + 1).List, (1, 2, 3, 4), 'map over a finite iterator';
is-deeply FSq.new.grep(* > 1).List, (2, 3), 'grep over a finite iterator';
is FSq.new.first(* > 1), 2, 'first(matcher) over a finite iterator';
ok FSq.new.first(* > 10) ~~ Nil, 'first(matcher) with no match is Nil';
is FSq.new.map(* + 1).elems, 4, 'elems of a map';

# Laziness: the callback runs only for the elements pulled.
{
    my $calls = 0;
    my @got = Sq.new.map({ $calls++; $_ }).head(3);
    is $calls, 3, 'map callback ran three times for head(3)';
}
{
    my $tests = 0;
    my $hit = Sq.new.first({ $tests++; $_ > 2 });
    is $hit, 3, 'first found the match';
    is $tests, 4, 'the matcher ran up to the first match only';
}

# A plain `Seq.new($iterator)` behaves the same.
is Seq.new(Endless.new).first(* > 5), 6, 'Seq.new(iterator).first(matcher)';
is-deeply Seq.new(Endless.new).map(* + 1).head(2).List, (1, 2), 'Seq.new(iterator).map.head';
