use Test;

# Candidate matching for plain positional signatures (#10107): nominal types,
# subsets and one-argument WhateverCode `where` clauses are checked directly,
# and every other shape keeps the general walk. These pin that both agree.

plan 14;

subset Small of Int where * < 100;
multi size(Small $x)                 { 'small' }
multi size(Int $x where * < 10_000)  { 'medium' }
multi size(Int $x)                   { 'large' }
multi size(Str $x)                   { 'str' }
is (size(5), size(5000), size(50_000), size('x')).join(','),
    'small,medium,large,str', 'subset, where and nominal candidates';

my $v = 5000;
is size($v), 'medium', 'a variable argument is matched by its value';
my @a = 1, 5000;
is @a.map({ size($_) }).join(','), 'small,medium', 'repeated dispatch from a block';

# A predicate naming an earlier parameter needs it bound.
multi between($lo, $x where * > $lo) { 'above' }
multi between($lo, $x)               { 'not above' }
is between(5, 7), 'above', 'where reads an earlier parameter (accept)';
is between(5, 3), 'not above', 'where reads an earlier parameter (reject)';

# A throwing predicate rejects the candidate.
multi signum(Any $x where * > 0) { 'positive' }
multi signum(Any $x)             { 'other' }
is signum('abc'), 'other', 'a predicate that throws rejects its candidate';
is signum(3), 'positive', 'the same predicate accepts';

# Arity and named arguments decline the short path.
multi arity(Int $x where * > 0)        { 'one' }
multi arity(Int $x, Int $y)            { 'two' }
multi arity(Int $x where * > 0, :$n!)  { 'named' }
is arity(1), 'one', 'one positional';
is arity(1, 2), 'two', 'two positionals';
is arity(1, :n(2)), 'named', 'a named argument';

# Undefined arguments and type objects.
multi defd(Int:D $x where * > 0) { 'defined' }
multi defd(Int:U $x)             { 'type object' }
is defd(Int), 'type object', 'smiley candidates see a type object';
is defd(4), 'defined', 'and a defined value';

# A lexical subset shadows an outer one of the same name.
subset Even of Int where * %% 2;
multi parity(Even $x) { 'even' }
multi parity(Int $x)  { 'odd' }
is (parity(2), parity(3)).join(','), 'even,odd', 'subset predicate per call';
{
    my subset Even of Int where * %% 3;
    multi lparity(Even $x) { 'by three' }
    multi lparity(Int $x)  { 'not by three' }
    is (lparity(9), lparity(4)).join(','), 'by three,not by three',
        'a lexical subset of the same name is its own';
}

done-testing;
