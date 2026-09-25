use Test;

# A functional-form reduction `[op](...)` is an ordinary term inside call
# parentheses, so an infix may follow it (#9328; Statistics::Distributions and
# Data::Generators write `sqrt([+]((@x X- mean(@x)) X** 2) / @x.elems)`).

plan 8;

sub f(*@a) { @a }

is f([+](1,2) + 1), [4], 'reduction followed by +';
is f([+](1,2) / 2), [1.5], 'reduction followed by /';
is f(1, [+](1,2) * 2), [1, 6], 'reduction in a later argument';
is f([+](1,2) - 1, 5), [2, 5], 'reduction then infix, then another argument';
is f([*](2,3) ~ "x"), ['6x'], 'reduction followed by ~';
my @x = 1, 2, 3, 4;
is-approx sqrt([+]((@x X- 2.5) X** 2) / @x.elems), 1.118033988749895,
    'the standard-deviation helper from the affected distributions';
is f([\+](1,2,3), 9), [1, 3, 6, 9], 'a bare triangle reduction still ends the argument';
is f([+](1,2)), [3], 'a bare reduction still ends the argument';
