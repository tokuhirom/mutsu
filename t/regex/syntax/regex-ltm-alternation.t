use v6;
use Test;

# ADR-0022 Slice 3 acceptance matrix (docs/adr/0022-regex-alternation-ltm-ranking.md
# §7), raku-verified 2026-08-09: `|` alternation ranks branches by
# (declarative-prefix length desc, leading-literal length ["litlen"] desc,
# declaration order asc), NOT by longest actual (full-branch) match. Each
# test below pins one line of that matrix.

# ties broken by litlen
"ab" ~~ / (\w\w) | 'ab' /;
ok !$0.defined, 'tie: literal branch beats capture branch on litlen';

"ab" ~~ / 'ab' | (\w\w) /;
ok !$0.defined, 'tie: literal branch (first) beats capture branch on litlen';

"/category/tree" ~~ / "/category/" (\w+) | "/category/tree" /;
ok !$0.defined, 'tie: full-literal branch (litlen 14) beats prefix+capture (litlen 10)';

"abc" ~~ / 'a' (\w\w) | 'abc' /;
ok !$0.defined, 'tie: literal branch beats capture branch (capture kills litlen)';

"abc" ~~ / 'a' \w\w | ('abc') /;
ok !$0.defined, 'tie: FIRST (non-capturing) branch beats capture branch (capture kills b2 litlen)';

"aab" ~~ / 'a' \w \w (<?>) | 'aa' \w /;
ok !$0.defined, 'tie: second (no-capture) branch wins via higher litlen (2 > 1)';

# capture groups transparent for length
"/xy" ~~ / "/" \w | ("/xy") /;
is ~$/, "/xy", 'capture branch wins on LENGTH (3 > 2) despite lower litlen';
ok $0.defined, 'capture branch wins: $0 is defined';

# groups/nested alternation extend litlen
"/category/tree" ~~ / "/category/" (\w+) | "/category/" [ 'tree' ] /;
ok !$0.defined, 'non-capturing group branch beats capture branch (group extends litlen)';

"/c/tree" ~~ / "/c/" (\w+) | "/c/" [ 'tree' | 'x' ] /;
ok !$0.defined, 'nested-alternation branch beats capture branch (all-pure-literal nested | extends litlen)';

# quantifiers: length yes, litlen no
"abab" ~~ / (\w+) | 'ab' ** 2 /;
ok !$0.defined, 'tie: first (capture) branch wins by declaration order (litlen tie 0-0)';

"aab" ~~ / 'a' | 'a' ** 1..2 'b' /;
is ~$/, "aab", 'second branch wins on LENGTH (3 > 1), litlen irrelevant';

# subrule descent (length + litlen)
{
    my token abb { 'abb' }
    "abb" ~~ / (\w+) | <abb> /;
    ok $<abb>.defined, 'subrule branch beats capture branch (subrule descent extends litlen)';
}

# code atoms
"abcd" ~~ / 'ab' { ; } \w\w | 'abc' /;
is ~$/, "abc", 'plain code block terminates the declarative prefix';

"abcd" ~~ / 'ab' <?{ True }> \w\w | 'abc' /;
is ~$/, "abcd", '<?{ }> code assertion is transparent (epsilon), first branch wins on length';

# lookahead
"abcd" ~~ / 'ab' <?before c> (\w\w) | 'abc' /;
is ~$/, "abc", 'positive lookahead extends litlen through the tie (3 beats 2)';

"abcde" ~~ / ab <![e]> cde | ab.. /;
is ~$/, "abcd", 'negated lookahead terminates the prefix without extending it';

# sequential alternation inside |
"food" ~~ / 'foo' | ['doof' || 'food'] /;
is ~$/, "foo", 'X || Y inside | only contributes its first branch to the declarative prefix';

"food" ~~ / 'foo' | ['food' || 'doof'] /;
is ~$/, "food", 'X || Y whose first branch matches wins on length';

# ws stopper
{
    my rule  r {\w+ '-'+}
    my token t {\w+ '-'}
    "abc---" ~~ /<r>|<t>/;
    is ~$/, "abc-", '<ws> stops the declarative prefix: the fully-declarative token wins';
    ok $<t>.defined, 'the winning branch is <t>, not <r>';
}

# fall to next best when the winner's tail fails
"food" ~~ / (f\w+) x | 'foo' /;
is ~$/, "foo", "ranking's top branch failing outer-context falls back to the next-ranked branch";

# within-branch backtracking preserved across the alternation
"aaab" ~~ / [ a+ | q ] ab /;
is ~$/, "aaab", 'plural ends per branch let the engine backtrack into a shorter end of the chosen branch';

# ranking never overrides leftmost-position scan
"xab" ~~ / 'ab' | b /;
is ~$/, "ab", 'the earlier subject position wins regardless of branch ranking';

# :i
"AB" ~~ m:i/ (\w\w) | 'ab' /;
ok !$0.defined, ':i literal still participates in litlen ranking (case-insensitive tie)';

# ratchet interaction unchanged (roast S05 486-489 already pass)
ok !('ab' ~~ / [ab | a ]: b /).defined, 'ratchet interaction with alternation ranking is unchanged';

# ADR-0022 Slice 5, raku-verified 2026-08-11: non-constant `$var` interpolation
# does not participate in LTM prefix/litlen ranking (Rakudo only inlines a
# `constant`'s value as a compile-time literal); a `constant`-interpolated
# value keeps participating exactly like a hand-written literal.
{
    # roast/S05-metasyntax/longest-alternative.t test 50's exact shape.
    constant $x = 'ab';
    is ~('ab' ~~ / a | b | $x /), 'ab',
        'constant-interpolated branch competes on length like a literal';

    my $y = 'ab';
    is ~('ab' ~~ / a | b | $y /), 'a',
        'non-constant $var interpolation does not count toward LTM';
}

{
    # A longer DECLARATIVE literal branch beats a non-constant interpolated
    # branch that would otherwise win on raw match length.
    my $z = 'abcd';
    is ~('abcd' ~~ / 'abc' | $z /), 'abc',
        'declarative literal branch beats a longer non-constant $var branch';

    # The same shape with a `constant` of identical text: the constant still
    # competes on length, since it participates like a literal.
    constant $w = 'abcd';
    is ~('abcd' ~~ / 'abc' | $w /), 'abcd',
        'constant-interpolated branch still wins on length like a literal';
}

# A plain (non-interpolated) alternation is unaffected by the non-constant
# marking machinery.
is ~('abab' ~~ / 'a' | 'ab' /), 'ab',
    'plain literal alternation (no interpolation) ranks normally';

done-testing;
