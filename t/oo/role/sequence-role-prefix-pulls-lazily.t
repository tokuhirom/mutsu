use Test;

# A class that `does Sequence` answers a prefix consumer (`head(n)`, `head`,
# a bare `first`, `AT-POS(i)`) by pulling only that prefix from its iterator,
# as Rakudo's `Sequence` does, so an endless iterator still answers (#15081).
# Checked against rakudo.

plan 12;

class Endless does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n++ }
}
class Sq does Sequence { method iterator { Endless.new } }

is-deeply Sq.new.head(3), (0, 1, 2).Seq, 'head(3) of an endless iterator';
is-deeply Sq.new.head(0), ().Seq, 'head(0) pulls nothing';
is Sq.new.head, 0, 'head without an argument is the first item';
is Sq.new.first, 0, 'a bare first is the first item';
is Sq.new.AT-POS(4), 4, 'AT-POS pulls up to the index';

class Finite does Iterator {
    has @.v = 1, 2;
    method pull-one { @!v ?? @!v.shift !! IterationEnd }
}
class FinSq does Sequence { method iterator { Finite.new } }

is-deeply FinSq.new.head(5), (1, 2).Seq, 'head past the end gives what there is';
is-deeply FinSq.new.head(1), (1,).Seq, 'head(1) of a finite iterator';
is-deeply FinSq.new.list, (1, 2), 'list still drains it';
is FinSq.new.elems, 2, 'elems still drains it';

class Hold does Sequence { has $.iterator }
my $held = Hold.new(iterator => Finite.new);
is-deeply $held.head(1), (1,).Seq, 'head(1) of a held iterator';
is-deeply $held.List, (2,), '...leaves the rest for a later read';
is FinSq.new.AT-POS(1), 2, 'AT-POS of a finite iterator';
