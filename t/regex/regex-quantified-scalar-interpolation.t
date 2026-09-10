use Test;

my $s = "z";
ok "(xy)" ~~ m/ $s? /, 'quantified interpolated scalar matches with zero occurrences';
is ~$/, "", 'the optional interpolated scalar produced an empty match';

my $p = "%";
ok "ab" ~~ m/ $p? b /, 'quantified interpolated scalar with a metachar value still matches';
is ~$/, "b", 'the metachar value is matched literally, not as a regex construct';

my $t = "abc";
ok "abcabc" ~~ m/ $t? /, 'a multi-char interpolated scalar quantifies as ONE atom';
is ~$/, "abc", 'the whole interpolated value matched, not just its last char';

my $u = "ab";
ok "ababab" ~~ m/ $u+ /, 'one-or-more quantifier on an interpolated scalar repeats the whole value';
is ~$/, "ababab", 'all repetitions of the interpolated value were consumed';

done-testing;
