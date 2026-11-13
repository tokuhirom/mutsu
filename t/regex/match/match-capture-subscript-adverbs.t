use Test;

# Subscript adverbs on a Match read its captures: `$/<x>:k`, `$<x>:v`,
# `$/[1]:kv`, and the `$<x>` / `$0` sugar takes every adverb, `:delete`
# included, also at the start of a statement (Red t/117).

plan 15;

'ab' ~~ /$<x>=a/;
is ($<x>:k), 'x', '$<x>:k';
is ~($<x>:v), 'a', '$<x>:v';
is ($<x>:p).key, 'x', '$<x>:p';
is ($/<x>:kv).elems, 2, '$/<x>:kv';
ok ($<x>:exists), '$<x>:exists';
ok ($<y>:!exists), '$<y>:!exists';

'ab' ~~ /(a)(b)/;
is ($/[1]:kv)[0], 1, '$/[1]:kv key';
is ~($/[1]:kv)[1], 'b', '$/[1]:kv value';
is ($/[0,1]:v).map(~*).join(','), 'a,b', '$/[0,1]:v';
is ($0:k), 0, '$0:k';

my $code = { $<x>:delete if .so; 'ran' };
is $code(0), 'ran', 'a statement-start $<x>:delete parses';
my $exists = { $<x>:exists if .so };
nok $exists(1), 'and :exists after it';

# `%N` is `$N.hash`, as `@N` is `$N.list`.
'ab' ~~ /((a))/;
is %0.^name, 'Map', '%0 is the hash of $0';
is (@0).map(~*).join, 'a', '@0 is the list of $0';
my $parsed = { %0:exists if .so };
pass 'a statement-start %0:exists parses';
