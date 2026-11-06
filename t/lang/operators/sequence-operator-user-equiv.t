use Test;
use lib 't/lib';
use StashClassThenSubs;
plan 4;

# A sequence operator the program declares with `is equiv(&infix:<...>)` sits at
# the sequence level, where the built-in token used to win over the declaration.
is infix:<^...>(1, 5), 'mu(5)', 'the declared operator is called by name';
is infix:<^...>(1, *), 'whatever:mu(9)', 'and its body reaches the declared candidate, not the built-in';
is (1 ^... 5), 'mu(5)', 'operator syntax calls the declared operator';

# A declaration without a precedence trait adds candidates and keeps the built-in.
multi sub infix:<...>(Str $a, Str $b where 'zzz') { 'custom' }
is (1, 2 ... 4).head(4).join(','), '1,2,3,4', 'the built-in ... still works for other operands';
