use Test;

# Reifying a Seq receiver runs its iterator's own `push-all`. That call has
# no call site of its own, so its parameters must not take the names of the
# outer method call's arguments: a sigilless `push-all(\target)` used to be
# bound as if `target` came from `.categorize(&prefix:<->)`'s `&prefix:<->`,
# and its buffer was then written back over the operator (#17935).

plan 5;

# The buckets as `key=values`, keys in numeric order (categorize keys are
# the mapper's own values, so an Int key makes an object hash).
sub buckets(%h) { %h.sort(*.key).map({ "{.key}={.value}" }).join(' ') }

class Sigilless does Iterator {
    has @.v = 3, 1, 2;
    method pull-one() { @!v ?? @!v.shift !! IterationEnd }
    method push-all(\target) { target.push($_) for @!v; @!v = (); IterationEnd }
}

is buckets(Seq.new(Sigilless.new).categorize(&prefix:<->)), '-3=3 -2=2 -1=1',
    'categorize with an operator callable over a sigilless push-all';
is buckets(Seq.new(Sigilless.new).categorize(&abs)), '1=1 2=2 3=3',
    'categorize with a named routine';
is (&prefix:<->)(5), -5, 'the operator is left intact afterwards';

my $n = 0;
sub count-it($x) { $n++; $x }
my @got = Seq.new(Sigilless.new).map(&count-it);
is-deeply @got, [3, 1, 2], 'map with a routine over a sigilless push-all';

sub negate($x) { -$x }
is buckets(Seq.new(Sigilless.new).categorize(&negate)), '-3=3 -2=2 -1=1',
    'categorize with a user sub';
