use v6;
use Test;

plan 1;

grammar InlineAdverbBracketScope {
    token TOP { $<element>=[ :i'first-'[line|letter]|before|after ] }
}

my $match = InlineAdverbBracketScope.subparse('After');
ok $match && $match<element>.Str eq 'After',
    ':i on a bracketed alternation scopes each alternative';
