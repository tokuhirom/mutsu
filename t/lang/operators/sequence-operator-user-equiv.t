use Test;
use lib 't/lib';
use StashClassThenSubs;
plan 3;

# A sequence operator the program declares with `is equiv(&infix:<...>)` sits at
# the sequence level, where the built-in token used to win over the declaration.
is infix:<^...>(1, 5), 'mu(5)', 'the declared operator is called by name';
is infix:<^...>(1, *), 'whatever:mu(9)', 'and its body reaches the declared candidate, not the built-in';
is (1 ^... 5), 'mu(5)', 'operator syntax calls the declared operator';
