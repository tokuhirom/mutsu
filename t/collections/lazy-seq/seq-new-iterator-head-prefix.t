use Test;

# `.head(n)` / `.head` / `.first` on a `Seq.new($iterator)` pull only the
# elements they need, whether or not the iterator claims `is-lazy` (#9353).

plan 11;

class Forever does Iterator {
    has $.i = 0;
    has @.log;
    method pull-one { @!log.push($!i); $!i++ }
}
class Three does Iterator {
    has $.i = 0;
    method pull-one { $!i < 3 ?? $!i++ !! IterationEnd }
}

is-deeply Seq.new(Forever.new).head(3), (0, 1, 2).Seq, 'head(n) on an unbounded iterator';
my \s = Seq.new(Forever.new);
is-deeply s.head(2), (0, 1).Seq, 'head(n) through a sigilless binding';
sub f(--> Seq:D) { Seq.new: Forever.new }
is-deeply f().head(3), (0, 1, 2).Seq, 'head(n) on a routine-returned Seq';

my $it = Forever.new;
my @got = Seq.new($it).head(3);
is-deeply $it.log.List, (0, 1, 2), 'head(3) calls pull-one exactly three times';

is Seq.new(Forever.new).head, 0, 'head with no argument';
is Seq.new(Forever.new).first, 0, 'first with no matcher';
is-deeply Seq.new(Three.new).head(10), (0, 1, 2).Seq, 'head(n) stops at IterationEnd';
is-deeply Seq.new(Three.new).head(0), ().Seq, 'head(0) pulls nothing';

my $s = Seq.new(Three.new);
$s.head(2);
throws-like { $s.List }, X::Seq::Consumed, 'head consumes the Seq';

my $c = Seq.new(Three.new);
$c.cache;
is-deeply $c.head(2), (0, 1).Seq, 'head on a cached Seq';

is-deeply Seq.from-loop({ 42 }).head(3), (42, 42, 42).Seq, 'head(n) on an infinite Seq.from-loop';
