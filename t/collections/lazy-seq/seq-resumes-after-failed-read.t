use Test;

# A deferred Seq whose non-consuming read dies keeps the list reified so far
# and resumes after the failing element, as rakudo's .cache does (#12048).

plan 18;

my $s = (1, 2, 3).map({ die "second" if $_ == 2; $_ });
throws-like { $s.elems }, Exception, message => /second/, 'the failing read dies';
is $s.elems, 2, 'the next read skips the failing element';
is $s.List.join(','), '1,3', 'and keeps the partial list';

my $r = (1, 2).map({ die "boom" });
throws-like { $r.elems }, Exception, message => /boom/, 'first failing element';
throws-like { $r.elems }, Exception, message => /boom/, 'second failing element';
is $r.elems, 0, 'then the Seq is exhausted, with no error';

my $n = 0;
my $o = (1, 2, 3).map({ die "once" if $n++ == 0; $_ * 10 });
throws-like { $o.elems }, Exception, message => /once/, 'map dies on the first element';
is $o.List.join(','), '20,30', 'the remaining elements are read afterwards';

my $g = (1, 2, 3, 4).grep({ die "g" if $_ == 3; True });
try $g.elems;
is $g.List.join(','), '1,2,4', 'grep resumes past the failing element';

class It does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n++; die "it" if $!n == 2; $!n > 4 ?? IterationEnd !! $!n }
}
my $i = Seq.new(It.new);
try $i.elems;
is $i.List.join(','), '1,3,4', 'an Iterator Seq resumes where the iterator stands';

my $c = (1, 2).map({ die "x" });
try $c.List;
dies-ok { $c.List }, "a consuming read of a fresh Seq still leaves it consumed";

# Every pull loop keeps its own progress (#12401): the chained, rw,
# typed-parameter, routine-callback and multi-parameter maps, and a grep over
# an Array, resume like the inline map. Expected values are the `raku` output.
my $chain = (1, 2, 3).map({ $_ }).grep({ die "second" if $_ == 2; True });
try $chain.elems;
is $chain.List.join(','), '1,3', 'a chained map/grep resumes';

my @rw = 1, 2, 3;
my $rw = @rw.map(-> $x is rw { die "second" if $x == 2; $x });
try $rw.elems;
is $rw.List.join(','), '1,3', 'an rw map resumes';

my $typed = (1, 2, 3).map(-> Int $x { die "second" if $x == 2; $x });
try $typed.elems;
is $typed.List.join(','), '1,3', 'a typed-parameter map resumes';

my $routine = (1, 2, 3).map(sub ($x) { die "second" if $x == 2; $x });
try $routine.elems;
is $routine.List.join(','), '1,3', 'a routine-callback map resumes';

my $pairs = (1, 2, 3, 4).map(-> $a, $b { die "second" if $a == 3; $a + $b });
try $pairs.elems;
is $pairs.List.join(','), '3', 'a multi-parameter map resumes after the failing group';

my @ga = 1, 2, 3, 4;
my $ga = @ga.grep({ die "g" if $_ == 3; True });
try $ga.elems;
is $ga.List.join(','), '1,2,4', 'a grep over an Array resumes';

my $head = (1, 2, 3).map({ die "second" if $_ == 2; $_ });
try $head[1];
is $head.List.join(','), '1,3', 'a subscript (prefix) read resumes';

