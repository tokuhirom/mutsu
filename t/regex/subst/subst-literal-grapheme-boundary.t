use Test;

plan 11;

my $ka = "\x[FF76]\x[FF9E]"; # half-width ka + voiced mark is one grapheme
my $half-kana = "\x[FF76]";

my $substitution = $ka;
$substitution ~~ s:g/"\x[FF76]"/Y/;
is $substitution, $ka, 's/// does not match a literal string inside a grapheme';

my $mutated = $ka;
my $mutate-result = $mutated.subst-mutate($half-kana, 'Y');
is $mutated, $ka, 'subst-mutate leaves the grapheme unchanged';
ok $mutate-result === Any, 'subst-mutate reports no match inside a grapheme';
my $matched = $ka ~ $half-kana;
my $match-result = $matched.subst-mutate($half-kana, 'Y');
is $matched, $ka ~ 'Y', 'subst-mutate still replaces a later whole grapheme';
ok $match-result ~~ Match, 'subst-mutate returns the whole-grapheme Match';

is $ka.split($half-kana).raku, '("ｶﾞ",).Seq', 'split does not split inside a grapheme';
is $ka.split([/z/, $half-kana]).raku, '("ｶﾞ",).Seq',
    'a string splitter in a mixed splitter list respects grapheme boundaries';
is ($ka ~ $half-kana).split($half-kana).raku, '("ｶﾞ", "").Seq',
    'split still matches the same literal at a later grapheme boundary';

is $ka.trans($half-kana => 'Y'), $ka, 'trans does not translate inside a grapheme';
is ($ka ~ $half-kana).trans($half-kana => 'Y'), $ka ~ 'Y',
    'trans still translates the same literal at a later grapheme boundary';

is $ka.subst($half-kana, 'Y', :g), $ka,
    'global string subst keeps the existing grapheme-boundary behavior';
