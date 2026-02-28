use Test;

plan 8;

{
    my $str = 'x' x 7;
    ok $str ~~ m/x||xx||xxxx/;
    is ~$/, 'x', 'sequential alternation picks first successful branch';
}

{
    my $str = 'x' x 7;
    my @list = <x xx xxxx>;
    ok $str ~~ m/ ||@list /;
    is ~$/, 'x', 'array interpolation preserves sequential branch order';
}

{
    is 'ab' ~~ / [ab || a ] b /,      'ab', 'backtracks into || when needed';
    is 'ab' ~~ / [ab || a ]: b /,     Nil,  'ratcheting atom prevents backtracking';
    is 'ab' ~~ / :r [ab || a ] b /,   Nil,  'pattern-level :r ratchets sequential alternation';
    is 'ab' ~~ / :r [ab || a ]:! b /, 'ab', 'token-level :! disables inherited ratchet';
}
