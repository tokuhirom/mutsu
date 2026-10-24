use Test;

# A hyper method call interpolates in a double-quoted string like a plain one
# when its chain ends in parentheses (`"@m>>.gist()"`, `"@m».gist()"`); it
# stayed literal text. Measured on rakudo 2026.09.

plan 7;

my @m = 1, 2;
is "x @m>>.gist() y", 'x 1 2 y', '>>.meth()';
is "x @m».gist() y", 'x 1 2 y', '».meth()';
is "x @m>>.gist y", 'x @m>>.gist y', 'without parentheses it stays text';
is "x @m>>.Str.join('-') y", 'x 1-2 y', 'a chain after the hyper call';
is "x @m>>.succ.join() y", 'x 23 y', 'a parenthesized last call';
my %h = a => 1;
is "%h>>.succ() z", "a\t2 z", 'over a hash';
is EVAL(Q[my @n = 3, 4; "x @n>>.gist() y"].AST), 'x 3 4 y', 'the RakuAST round trip';
