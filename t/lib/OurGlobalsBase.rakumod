unit module OurGlobalsBase;

# `our` declarations live in the package, not in whatever lexical scope happened
# to trigger the module load.
our constant answer = 42;
our $greeting = 'hi';
our @items = 1, 2, 3;
our %config = mode => 'fast';
