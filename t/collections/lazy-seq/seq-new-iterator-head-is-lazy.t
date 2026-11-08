use Test;
# `Seq.new($iterator).head(n)` is a lazy Seq: rakudo pulls nothing until the
# result is consumed, then exactly n values (BinaryHeap's `extract(n)` relies on
# it to leave the heap untouched). Identical under rakudo.
plan 9;

class Counter does Iterator {
    has $.n is rw = 0;
    has @.log;
    method pull-one() { @!log.push(++$!n); $!n }
}

my $c = Counter.new;
my $s = Seq.new($c).head(3);
is $c.log.elems, 0, 'head(3) pulls nothing at the call';
is $s.List.join(','), '1,2,3', 'consuming yields the first three values';
is $c.log.elems, 3, 'exactly three pulls';

my $d = Counter.new;
my $h = Seq.new($d).head(2);
is $h.elems, 2, 'elems of a lazy head';
is $d.log.elems, 2, 'elems pulled exactly two';
ok $h.Bool, 'a consumed head still answers Bool';

my $e = Counter.new;
is Seq.new($e).head, 1, 'head without a count is the first value';
is $e.log.elems, 1, 'and pulls once';

my $z = Counter.new;
is Seq.new($z).head(0).List.elems, 0, 'head(0) is empty';
