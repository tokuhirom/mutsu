use v6;
use Test;

plan 8;

# Substitutions and transliterations are carried through the executing
# finalization (ADR-15760 slice 2) and keep every adverb.

my $s = 'abcabc';
$s ~~ s/b/X/;
is $s, 'aXcabc', 's/// replaces the first match';
$s = 'aaa';
$s ~~ s:g/a/b/;
is $s, 'bbb', 's:g///';
$s = 'abcabc';
$s ~~ s:2nd/b/Y/;
is $s, 'abcaYc', 's:2nd///';
$s = 'abc';
$s ~~ s[b] = uc $/;
is $s, 'aBc', 'assignment-form s[]';
$s = 'a1';
$s ~~ s[\d] += 2;
is $s, 'a3', 'metaassign-form s[] +=';
is (S/b/Z/ given 'abc'), 'aZc', 'S/// leaves the topic alone';
$s = 'abc';
$s ~~ tr/a..c/A..C/;
is $s, 'ABC', 'tr///';
is (TR/a..c/x/ given 'abc'), 'xxx', 'TR///';
