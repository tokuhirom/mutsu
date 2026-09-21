use Test;

# List::Divvy uses `grep(&is-prime)` in its public test suite. The builtin
# already supports call syntax, but its routine value must also be available
# through the `&` sigil.
plan 3;

ok &is-prime ~~ Callable, '&is-prime resolves to a callable';
is &is-prime(7), True, '&is-prime can be called directly';
is (1..10).grep(&is-prime).join(','), '2,3,5,7',
    'grep accepts the builtin is-prime routine value';
