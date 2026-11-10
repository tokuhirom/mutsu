use Test;

# `Seq.new($iterator).head($n).iterator` answers the whole Iterator protocol
# (ParaSeq's `.IterationBuffer` calls `.iterator.push-all`), pulls lazily and
# never pulls the wrapped iterator past `$n` elements.
plan 14;

my @log;
my class Counter does Iterator {
    has $.n is rw = 0;
    method pull-one { @log.push($!n + 1); $!n < 100 ?? ++$!n !! IterationEnd }
}

my $seq = Seq.new(Counter.new).head(3);
is @log.elems, 0, "building the head Seq pulls nothing";
my $it = $seq.iterator;
is @log.elems, 0, "taking its iterator pulls nothing";
ok $it ~~ Iterator, "it is an Iterator";

my $buf = IterationBuffer.new;
is $it.push-exactly($buf, 2), 2, "push-exactly answers the count";
is $buf.List.raku, '(1, 2)', "and appended those elements";
is $it.skip-one, 1, "skip-one consumes the third";
is $it.pull-one.raku, 'IterationEnd', "then the head is exhausted";
is @log.elems, 3, "the wrapped iterator was pulled exactly three times";

my $all = IterationBuffer.new;
Seq.new(Counter.new).head(4).iterator.push-all($all);
is $all.elems, 4, "push-all stops at the head count";

my $it2 = Seq.new(Counter.new).head(5).iterator;
is $it2.skip-at-least(2), 1, "skip-at-least";
is $it2.skip-at-least-pull-one(1), 4, "skip-at-least-pull-one";

is Seq.new(Counter.new).head(2).List.raku, '(1, 2)', ".List";
is Seq.new(Counter.new).head(4).iterator.sink-all.raku, 'IterationEnd', "sink-all";
is Seq.new(Counter.new).head(3).Array.raku, '[1, 2, 3]', ".Array";
