use Test;

plan 3;

is do { defined(42) ?? 1 !! 2 }, 1, 'ternary after known call parses in do block';

sub optional($p?) { defined($p) ?? $p !! 'undef' }
is optional('abc'), 'abc', 'ternary after known call parses in sub body';
is optional(), 'undef', 'optional arg + ternary else branch works';
