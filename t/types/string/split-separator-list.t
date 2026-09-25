use Test;

# Multi-separator `.split` caches each separator's next occurrence (#9145).
# These pin that the cache keeps the earliest-match / longest-on-tie choice.

plan 14;

is-deeply "a,b;c,d".split([",", ";"]).List, ("a", "b", "c", "d"), 'two string separators';
is-deeply "a,b,c".split([",", "#"]).List, ("a", "b", "c"), 'absent separator never wins';
is-deeply "a::b:c".split([":", "::"]).List, ("a", "b", "c"), 'longest separator wins a tie';
is-deeply "a::b:c".split(["::", ":"]).List, ("a", "b", "c"), 'tie order does not matter';
is-deeply "xabcx".split(["bc", "ab"]).List, ("x", "cx"), 'earliest start wins over a later overlap';
is-deeply "a;b,c;d,e".split([",", ";"], 3).List, ("a", "b", "c;d,e"), 'limit';
is-deeply "a,b;c".split([",", ";"], :k).List, ("a", 0, "b", 1, "c"), ':k reports the separator index';
is-deeply "a,b;c".split([",", ";"], :v).List, ("a", ",", "b", ";", "c"), ':v reports the separator';

my $big = "a," x 20000;
my @p = $big.split([",", ";"]);
is @p.elems, 20001, 'many pieces with an absent separator';
is @p[19999], "a", 'last piece intact';

is-deeply "a,b;c,d".split([/","/, ";"]).List, ("a", "b", "c", "d"), 'regex + string list';
is-deeply "a1b22c".split([/\d+/, "b"]).List, ("a", "", "", "c"), 'regex list, adjacent separators';
is-deeply "a,b;c".split([/","/, ";"], :k).List, ("a", 0, "b", 1, "c"), 'regex list :k';
is ("x," x 5000).split([/","/, ";"]).elems, 5001, 'regex list, many pieces';
