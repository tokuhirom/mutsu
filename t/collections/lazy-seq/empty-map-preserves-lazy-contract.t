use Test;

# An empty source never invokes a map callback and still returns a Seq. This
# pins the no-deferred-source fast path used by empty constructor attributes
# (bench-ctor's `@!resources.map(*.flat)` shape).
plan 4;

my @empty;
my $calls = 0;
my $mapped = @empty.map({ $calls++; die 'must not run' });

is $mapped.^name, 'Seq', 'empty Array.map returns a Seq';
is $calls, 0, 'constructing an empty map does not invoke its callback';
is-deeply $mapped.List, (), 'consuming the empty map remains empty';
is $calls, 0, 'consuming an empty map does not invoke its callback';
