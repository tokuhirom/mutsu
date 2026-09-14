use Test;

plan 3;

# List::Allmax exercises a Callable named parameter after a positional slurpy.
sub callback(*@items, Callable :&by) {
    &by(2 => <a b>);
}

is callback(:by(+*.value)), 2,
    'a named Callable after a positional slurpy accepts WhateverCode';

sub passthrough(*@items, Int :$value) {
    $value;
}

is passthrough(:value('text')), 'text',
    'a named type constraint after a positional slurpy is not enforced';

sub constrained(*@items, Int :$value where * > 0) {
    $value;
}

dies-ok { constrained(:value(-1)) },
    'a where constraint after a positional slurpy is still enforced';
