use v6;
use Test;

# An integral Rat index is numified to an Int before a lazy positional read.
# This must pull only the needed prefix, not try to reify the infinite gather.
plan 1;

my $values = gather {
    for 0 .. * -> $value {
        take $value;
    }
};

is $values[(10 + 2) / 2], 6, 'integral Rat index reads a lazy sequence';

done-testing;
