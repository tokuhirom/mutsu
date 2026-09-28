use Test;

plan 5;

# A thunked `xx` (lhs re-evaluated per repetition) returns a Seq, like `1 xx 2`.
my $i = 0;
is ($i++ xx 2).^name, 'Seq', 'thunked xx with a literal count is a Seq';
is ($i++ xx 2).raku, '(2, 3).Seq', 'its .raku shows the Seq';
is (1 xx 2).^name, 'Seq', 'non-thunked xx is a Seq';

my $n = 3;
is ($i++ xx $n).^name, 'Seq', 'thunked xx with a variable count is a Seq';
is-deeply ($i++ xx 2).list, (7, 8), 'the items are still re-evaluated';
