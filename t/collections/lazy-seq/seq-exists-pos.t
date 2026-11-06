use Test;

# From Iter::Able (t/25-fill-undef): `$seq[$i]:exists` reads the Seq's cache.
plan 7;

my \s = (5, 77).Seq;
ok s[0]:exists, 'first slot of a Seq exists';
ok s[1]:exists, 'second slot exists';
nok s[2]:exists, 'a slot past the end does not exist';
ok s.EXISTS-POS(0), 'EXISTS-POS agrees';
my $t = (5, 77).Seq;
ok $t[0]:exists, 'a scalar-held Seq answers too';
ok $t[0]:exists, 'asking twice does not consume it';
is $t[1], 77, 'the Seq still reads';
