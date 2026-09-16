use v6;
use Test;

# `m:pos(EXPR)/.../` (and `:p`, `:continue`/`:c`) previously only recognized a
# compile-time-literal integer argument -- `arg.and_then(|s|
# s.trim().parse::<usize>().ok())` in src/parser/primary/regex/adverbs.rs
# silently dropped any other argument, so a dynamic offset like `$!pos`
# (updated at runtime) was discarded and the match anchored at `$/.to` (or 0)
# instead, per GH issue #8515.
#
# Fixed by parsing a non-literal argument as a standalone Raku expression
# (Expr::MatchRegexDynamicAdverbs) that is re-evaluated at every match instead
# of being baked into the regex's constant Value.

plan 10;

class Decoder {
    has Str $.text is required;
    has int $!pos = 0;
    method go {
        $!pos = 6;
        return $!text ~~ m:p($!pos)/ 0x <[0..9]>+ p '+' <[0..9]>+ /;
    }
}
is Decoder.new(text => '.Date(0x0p+0)').go, '0x0p+0',
    "dynamic :p(\$!pos) attribute argument anchors the match (issue #8515)";

my $pos = 6;
$_ = '.Date(0x0p+0)';
is m:p($pos)/ 0x <[0..9]>+ p '+' <[0..9]>+ /, '0x0p+0',
    "bare m:p(\$var)/.../ against \$_ also honors the dynamic position";

my $cpos = 2;
my $cmatch = 'xaxa' ~~ m:c($cpos)/ a /;
is $cmatch.from, 3,
    ":c(\$var)/ dynamic :continue argument searches starting at the given offset";

is-deeply ('abcdef' ~~ m:p(2)/ c /).so, True,
    "a literal :p(N) argument still works unchanged";

my $badpos = 0;
is-deeply '.Date(0x0p+0)' ~~ m:p($badpos)/ 0x <[0..9]>+ p '+' <[0..9]>+ /, False,
    "a dynamic position that doesn't line up with the pattern fails to match";

my $oob = 999;
is-deeply 'abc' ~~ m:p($oob)/ a /, False,
    "a dynamic position past the end of the string fails to match";

# The position expression is re-evaluated on every call, not baked in once.
my @seen;
for 6, 0 -> $p {
    my $s = '.Date(0x0p+0)';
    @seen.push(($s ~~ m:p($p)/ 0x <[0..9]>+ p '+' <[0..9]>+ /).so);
}
is-deeply @seen, [True, False],
    "the dynamic position is re-evaluated fresh on each match, not cached";

# `:continue` with a dynamic argument that fails to resolve as an expression
# at all falls back to the same "no explicit position" behavior as before.
$_ = 'xabc';
is-deeply so(m/ a /), True, "sanity: plain m// still matches";

# `.so` on the dynamic-:pos match result behaves like an ordinary Match.
$_ = '.Date(0x0p+0)';
my $p2 = 6;
my $m = m:p($p2)/ 0x <[0..9]>+ p '+' <[0..9]>+ /;
isa-ok $m, Match, "a successful dynamic-:pos match still returns a Match object";
is $m.Str, '0x0p+0', "...with the expected matched text";
