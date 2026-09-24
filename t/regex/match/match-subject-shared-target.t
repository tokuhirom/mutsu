use Test;

# Regex entry points share one MatchTarget per `Str` payload (#9144): repeated
# `~~`, `.match(:c/:p)` and `.contains(rx)` calls on the same long string reuse
# the subject instead of copying it per call. These pin that the sharing is
# invisible: every answer is the one a fresh copy would give.

plan 21;

# Long enough (>= 256 bytes) to take the cached path.
my $long = ("ab," x 200) ~ "END";

# Repeated `~~` against one payload.
for ^3 {
    ok $long ~~ /END/, "~~ finds the tail on repeat {$_}";
}
is ~$/, 'END', '$/ holds the last match';
is $/.from, 600, '.from is a char offset into the shared subject';
is $/.orig, $long, '.orig is the whole subject';

# A `.match(:p)` tokenizer loop over the same payload.
my $p = 0;
my $n = 0;
while $long.match(/'ab,'/, :p($p)) -> $m {
    $p = $m.to;
    $n++;
}
is $n, 200, ':p tokenizer loop sees every token';
is $long.match(/b/, :c(10)).from, 10, ':c searches from the given position';

# `.contains(rx)` on the same payload, with and without a start position.
ok $long.contains(/END/), '.contains(rx) finds the tail';
nok $long.contains(/'ab,ab,' $/), '.contains(rx) respects the end anchor';
ok $long.contains(/<?after ','>END/, 590), '.contains(rx, $pos) keeps lookbehind context';

# Value semantics: appending after a match must not disturb the old Match,
# and the next match sees the new text.
my $s = "x" x 300;
my $m = $s ~~ /x/;
$s ~= "YZ";
is $m.orig.chars, 300, 'an earlier Match keeps its own subject after ~=';
ok $s ~~ /YZ$/, 'a match after ~= sees the appended text';
is $/.orig.chars, 302, '... and its .orig is the new string';

# A grammar parse stamps its class on its own run only.
grammar G { token TOP { <[x]>+ } }
my $g = "x" x 300;
isa-ok G.parse($g), G, 'grammar parse yields a G cursor';
my $plain = $g ~~ /x+/;
is $plain.^name, 'Match', 'a later plain match of the same string is a Match';

# `.comb(rx, :match)` Matches share one subject.
my @ms = $long.comb(/'ab,'/, :match);
is @ms.elems, 200, 'comb :match finds every match';
is @ms[5].from, 15, 'comb :match .from';
is @ms[5].orig, $long, 'comb :match .orig is the whole subject';

# `.prematch` / `.postmatch`, including non-ASCII subjects.
my $u = ("あい" x 150) ~ "X" ~ ("う" x 10);
$u ~~ /X/;
is $/.prematch.chars, 300, '.prematch on a non-ASCII subject';
is $/.postmatch, "う" x 10, '.postmatch on a non-ASCII subject';
