use Test;

# ADR-0125: a `|` branch ranked from a real match is measured by a compiled
# NFA. Each case ranks a subrule branch `<x>` against a literal and checks
# which one wins, so it pins the measured prefix of one NFA construct.
# Expected values checked against `raku`.

plan 21;

sub winner(Grammar $g, Str $s) {
    my $m = $g.subparse($s);
    $m ?? ($m<x>:exists ?? "x:{~$m}" !! "lit:{~$m}") !! 'none'
}

# `** {code}` is a fate, with or without a separator.
grammar Q1 { token TOP { <x> | 'a,' }; token x { 'a' ** {3} % ',' } }
is winner(Q1, "a,a,a"), 'lit:a,', 'a separated code-counted quantifier is a fate';

grammar Q2 { token TOP { <x> | 'a,a' }; token x { 'a'+ % ',' } }
is winner(Q2, "a,a,a"), 'x:a,a,a', '`+ %` loops through the separator';

grammar Q3 { token TOP { <x> | 'a,a,' }; token x { 'a'+ %% ',' } }
is winner(Q3, "a,a,"), 'lit:a,a,', '`%%` ties with the literal, which wins on litlen';

# `||`: its first branch, plus an ε bypass.
grammar Q5 { token TOP { <x> | 'abc' }; token x { 'a' [ 'x' || 'b' ] 'c' 'd' } }
is winner(Q5, "abcd"), 'lit:abc', 'only the first `||` branch is declarative';

# `<?before X>` measures X, then ends the path.
grammar Q6 { token TOP { <x> | 'ab' }; token x { 'a' <?before 'bcd'> } }
is winner(Q6, "abcd"), 'x:a', 'a lookahead is measured through';

# `<?{ }>` is a zero-width pass; a plain block is a fate.
grammar Q7 { token TOP { <x> | 'ab' }; token x { 'a' <?{ True }> 'bc' } }
is winner(Q7, "abc"), 'x:abc', 'a code assertion does not end the prefix';
grammar Q8 { token TOP { <x> | 'ab' }; token x { 'a' { } 'bc' } }
is winner(Q8, "abc"), 'lit:ab', 'a code block ends the prefix';

# `:i` inside the subrule body.
grammar Q9 { token TOP { <x> | 'AB' }; token x { :i 'ab' 'c' } }
is winner(Q9, "ABC"), 'x:ABC', ':i applies inside the inlined body';

# A builtin `<name>` leaf.
grammar Q10 { token TOP { <x> | 'ab' }; token x { <ident> } }
is winner(Q10, "abc"), 'x:abc', 'a builtin subrule is measured by the matcher';

# A trailing `$` only accepts at the end of the subject.
grammar Q11 { token TOP { <x> | 'a' }; token x { 'a' $ } }
is winner(Q11, "a"), 'x:a', 'an end anchor accepts at the end';

# The NFA ignores `:ratchet`, like Rakudo's: `\w+` backs off for the 'c', so
# `<x>` ranks first, and then its real, ratcheted match fails.
grammar Q12 { token TOP { <x> | 'ab' }; token x { \w+ 'c' } }
is winner(Q12, "abbbc"), 'lit:ab', 'a ratcheted quantifier is measured without ratchet';

# The recursion cut (#9617).
grammar Q13 { token TOP { <x> | 'aab' }; token x { 'a' <x>? 'b' | 'q' } }
is winner(Q13, "aabb"), 'lit:aab', 'a recursive call ends its path';

# A rule's `<.ws>` is a fate.
grammar Q14 { rule TOP { <x> | 'a b' }; rule x { 'a' 'b' 'c' } }
is winner(Q14, "a b c"), 'lit:a b ', 'sigspace whitespace is a fate';

# A loop over an alternation reaches the longer branch.
grammar Q15 { token TOP { <x> | 'ab' }; token x { [ 'a' | 'abc' ]* 'd' } }
is winner(Q15, "aabcd"), 'x:aabcd', 'every path of a looped alternation is followed';

# #9637: a bounded `** min..max` never measures past `min(min+1, max)`
# repeats, and (like any quantifier) never contributes to `litlen` either —
# so a same-or-shorter literal sibling wins the `litlen` tie-break. Checked
# against `raku` (subject is 'a' x 5 throughout).
grammar Q16 { token TOP { <x> | 'aaa' }; token x { 'a' ** 2..4 } }
is winner(Q16, "aaaaa"), 'lit:aaa', 'a bounded range measures min(min+1,max), not max';

grammar Q17 { token TOP { <x> | 'aaa' }; token x { 'a' ** 3 } }
is winner(Q17, "aaaaa"), 'lit:aaa', 'an exact count never contributes to litlen either';

grammar Q18 { token TOP { <x> | 'aaa' }; token x { 'a' ** 0..4 } }
is winner(Q18, "aaaaa"), 'lit:aaa', 'a min of 0 caps the measured prefix at 1';

grammar Q19 { token TOP { <x> | 'aa' }; token x { 'a' ** 1..4 } }
is winner(Q19, "aaaaa"), 'lit:aa', 'min(min+1,max) still ties the litlen tie-break';

grammar Q20 { token TOP { <x> | 'aa' }; token x { 'a' ** 2..4 } }
is winner(Q20, "aaaaa"), 'x:aaaa', 'a genuinely longer measured prefix still wins outright';

# The #9617 shape, as `regex` and as `token`.
grammar R1 { token TOP { <A> }; regex A { '{' [ <A> | . ]*? '}' } }
grammar R2 { token TOP { <A> }; token A { '{' [ <A> | . ]*? '}' } }
my $s = '{' ~ ('{ab{c}d}' x 64) ~ '}';
is R1.parse($s)<A>.to, $s.chars, 'a recursive frugal regex parses';
is R2.parse($s)<A>.to, $s.chars, 'a recursive frugal token parses';
