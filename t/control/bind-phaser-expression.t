use Test;

plan 3;

my $begin-runs = 0;
my $begin-value := BEGIN {
    $begin-runs = 1;
    42
};
is $begin-value, 42, ':= keeps the value of a BEGIN expression';
is $begin-runs, 1, 'the BEGIN expression body runs for a binding';

my $init-value := INIT {
    state $init-runs = 0;
    ++$init-runs
};
is $init-value, 1, ':= keeps the value of an INIT expression';
