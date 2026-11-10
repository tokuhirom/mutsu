use Test;

# Found checking LLM::Chat: rakudo's Uni is no List, so .list/.List/.values are Seqs.
plan 6;

my $u = "a\x[301]b".NFC;
isa-ok $u.list, Seq, '.list is a Seq';
isa-ok $u.List, Seq, '.List is a Seq';
isa-ok $u.values, Seq, '.values is a Seq';
is $u.list.join(","), "225,98", '.list holds the codepoints';
is "a".NFC.list.raku, "(97,).Seq", '.list.raku';
is $u.Array.elems, 2, '.Array is unchanged';
