use Test;

plan 7;

grammar G { token TOP { (\w+) <.ws> $<n>=(\d+) } }
my $m = G.parse("abc 42");
is $m.Str, 'abc 42', 'a grammar parse result stringifies as its match';
is $m.from, 0, '.from';
is $m.to, 6, '.to';
is $m<n>.Str, '42', 'a named capture of a grammar match';
is $m[0].Str, 'abc', 'a positional capture of a grammar match';

ok $m.Bool, 'and .Bool';
is $m.pos, 6, 'and .pos';
