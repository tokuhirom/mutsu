use Test;

# The iterator of a `Seq.from-loop` Seq is an `Iterator` for type checks and
# bindings (#13370: Qwiratry's `TypedIterator::Wrapper.new(Seq.from-loop(...).iterator)`).

plan 10;

sub takes(Iterator $it) { 'bound' }
sub takes-defined(Iterator:D $it) { 'bound' }

my $n = 0;
my $seq = Seq.from-loop({ $n++ < 3 ?? $n !! IterationEnd });
my $it = $seq.iterator;
ok $it ~~ Iterator, 'a from-loop iterator smartmatches Iterator';
is takes($it), 'bound', 'it binds to an Iterator parameter';
is takes-defined($it), 'bound', 'and to an Iterator:D parameter';
is $it.pull-one, 1, 'it pulls its first value';
is $it.pull-one, 2, 'then the next';

class Wrapper {
    has Iterator $.source;
    method new(Iterator:D $source) { self.bless(:$source) }
    method pull-one() { $!source.pull-one }
}
my $m = 0;
my $w = Wrapper.new(Seq.from-loop({ $m++ < 2 ?? $m * 10 !! IterationEnd }).iterator);
is $w.pull-one, 10, 'an Iterator attribute accepts it';
is $w.pull-one, 20, 'and keeps pulling';

# A body that answers IterationEnd ends the iterator for good.
my $k = 0;
my $it2 = Seq.from-loop({ $k++ < 1 ?? 'only' !! IterationEnd }).iterator;
is $it2.pull-one, 'only', 'the first pull yields the body value';
ok $it2.pull-one =:= IterationEnd, 'the next ends the iterator';
ok $it2.pull-one =:= IterationEnd, 'and it stays ended without running the body again';
