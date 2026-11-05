use v6;
use Test;

plan 5;

# `m:p($pos)/.../` and `m:c($pos)/.../`, whose adverb argument is an expression,
# are carried through finalization as the parser built them, the code inside the
# regex included (ADR-15760 slice 2, #8033).

my $text = 'xxabcabc';
my $pos = 2;
ok $text ~~ m:p($pos)/ abc /, ':p with a variable offset matches there';
is $/.from, 2, 'the match starts at the offset';

$pos = 5;
ok $text ~~ m:c($pos)/ abc /, ':c scans from the offset';

my $seen = 0;
$pos = 2;
ok $text ~~ m:p($pos)/ a { $seen++ } b /, ':p with a code block in the regex';
is $seen, 1, 'the code block ran once';
