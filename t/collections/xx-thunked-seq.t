use Test;

plan 6;

my $i = 0;
is ($i++ xx 2).^name, 'Seq', 'thunked `$i++ xx 2` is a Seq';
is ($i++ xx 2).raku, '(2, 3).Seq', 'thunked xx re-evaluates and .raku shows Seq';
is (1 xx 2).^name, 'Seq', 'plain `1 xx 2` is a Seq';
is (rand xx 2).^name, 'Seq', '`rand xx 2` is a Seq';
is (++$i xx 3).List.raku, '(5, 6, 7)', 'prefix increment re-evaluated per repetition';
my @a = 1, 2, 3;
is (@a.shift xx 2).^name, 'Seq', '`@a.shift xx 2` is a Seq';
