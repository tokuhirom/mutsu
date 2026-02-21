use Test;
use Test::Util;

plan 8;

# Integer equality
is-eqv 42, 42, 'integers are eqv';

# String equality
is-eqv "hello", "hello", 'strings are eqv';

# Bool equality
is-eqv True, True, 'booleans are eqv';

# Nil equality
is-eqv Nil, Nil, 'Nil is eqv to Nil';

# Array/List equality
is-eqv (1, 2, 3), (1, 2, 3), 'lists are eqv';

# Nested structure
is-eqv (1, (2, 3)), (1, (2, 3)), 'nested lists are eqv';

# Hash equality
is-eqv {a => 1, b => 2}, {a => 1, b => 2}, 'hashes are eqv';

# Rat equality
is-eqv 1/3, 1/3, 'rats are eqv';
