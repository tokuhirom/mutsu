use v6;
use Test;

plan 2;

grammar OptionalOrderedAlternative {
    rule TOP { '(' [ <literal>? || <fallback> ] ')' }
    token literal { 'x' }
    token fallback { '@' \d+ }
}

my $match = OptionalOrderedAlternative.subparse('(@42)');
ok $match && $match.pos == 5,
    'an ordered alternative falls through after an optional empty branch';
is $match<fallback>.Str, '@42',
    'the fallback branch captures the input after the optional branch';
