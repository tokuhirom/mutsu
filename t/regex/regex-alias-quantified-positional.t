use Test;

plan 8;

is ("12" ~~ / $<x>=(\d) /).list.elems, 0,
    'an unquantified named capture group takes no positional slot';
is ("12" ~~ / [ $<x>=(\d) ]+ /).list.elems, 0,
    'a repeated named capture group takes no positional slot';
is ("1,2" ~~ / [ $<x>=(\d) ] +% ',' /).list.elems, 0,
    'a separated named capture group takes no positional slot';
my $nested = "1.2;3" ~~ / [ [ $<x>=(\d) ] +% '.' ] +% ';' /;
is $nested.list.elems, 0,
    'nested separated groups take no positional slots';
is $nested<x>.elems, 3, 'nested named captures remain available';
is ("123" ~~ / $<x>=(\d)+ /).list.elems, 0,
    'a directly quantified named alias takes no positional slot';
is ("" ~~ / [ $<x>=(\d) ]* /).list.elems, 0,
    'zero iterations reserve no positional slot for a named alias';
is ("12" ~~ / [ $<x>=(\d) ]+ (\d) /).list.elems, 1,
    'a following unnamed group keeps the first positional slot';
