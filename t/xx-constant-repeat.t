use Test;

plan 2;

# A small scalar constant count is known while compiling `xx`, so it can run
# its re-evaluated left side directly in the enclosing frame.
constant REPEATS = 10;
my $calls = 0;
my @values = $calls++ xx REPEATS;

is @values.elems, REPEATS, 'constant-count xx produces every repetition';
is $calls, REPEATS, 'constant-count xx re-evaluates its left side';
