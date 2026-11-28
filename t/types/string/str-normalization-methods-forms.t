use Test;

plan 7;

# `.NFD` / `.NFC` / `.NFKD` / `.NFKC` on a Str read its text where it lies and
# build the Uni from the normalized codepoints. Every answer is rakudo 2026.11's.

is 'abc'.NFD.list.join(','), '97,98,99', 'an ASCII Str';
is 'abc'.NFD.^name, 'NFD', 'the result is an NFD';
is "\x[e9]".NFD.list.join(','), '101,769', 'a composed character decomposes';
is "e\x[301]".NFC.list.join(','), '233', 'NFC composes';
is "\x[fb01]".NFKD.list.join(','), '102,105', 'NFKD splits a ligature';
is "\x[fb01]".NFKC.^name, 'NFKC', 'NFKC names its form';
my $s = "a\x[e9]";
is $s.NFD.elems, 3, 'a Str in a variable';
