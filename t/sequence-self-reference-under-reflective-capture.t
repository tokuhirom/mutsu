use Test;

# A self-referential sequence — the generator calls a routine that reads the
# very array the sequence is bound to — must keep working when the program
# contains reflective name access (`CALLER::` / `EVAL` / symbolic deref).
#
# Such a program makes every closure capture its whole environment by value, and
# the generator is created while `@primes` is still the hoisted empty array. If
# that snapshot is re-imposed on the live environment at each pull, the routine
# the generator calls sees the empty array instead of the assigned sequence.

plan 4;

# Never called: its only job is to make reflective name access possible, which
# is process-global.
sub uses-caller() { CALLER::<$x> }

my @primes = 2, 3, 5, -> $p { ($p + 2, $p + 4 ... &is-prime)[*-1] } ... *;
sub is-prime($n) { $n %% none @primes ...^ * > sqrt $n }

is-deeply @primes[0 .. 5].List, (2, 3, 5, 7, 11, 13),
    'self-referential sequence extends past its eager seeds';
is-deeply (2 .. 20).grep(&is-prime).List, (2, 3, 5, 7, 11, 13, 17, 19).List,
    'the routine reading the sequence sees the assigned list';

# An ordinary generator delegating to a named routine is unaffected.
my @powers = 1, 2, -> $p { dbl($p) } ... *;
sub dbl($p) { $p * 2 }
is-deeply @powers[0 .. 4].List, (1, 2, 4, 8, 16),
    'generator delegating to a named routine still works';

# A genuine capture must still shadow a same-named caller lexical.
my $step = 10;
my @stepped = do {
    my $step = 3;
    (1, { $^a + $step } ... * > 10);
};
is-deeply @stepped.List, (1, 4, 7, 10, 13),
    'the generator keeps its own captured $step, not the outer one';
