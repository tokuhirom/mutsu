use Test;

# A lookaround runs on the compiled regex engine (ADR-0135 §8, Slice E,
# nineteenth part): its body is a nested run that shares the enclosing regex's
# `:my` lexicals and none of its captures, as rakudo's lookaround cursor does.
# Expected values are rakudo's.

plan 12;

my $seen;
"ab" ~~ / (a) <?before b { $seen = $0.defined }> /;
is $seen, False, 'code in a lookahead does not see the enclosing captures';

"ab" ~~ / a <?before b { $seen = $/.from }> /;
is $seen, 1, 'the lookahead body is a match of its own, starting where it is tried';

"xab" ~~ / x (a) <?after a { $seen = $0.defined }> b /;
is $seen, False, 'code in a lookbehind does not see the enclosing captures';

ok "ab" ~~ / a :my $v = 1; <?before b { $v = 5 }> { $seen = $v } /,
    'a lookahead whose code writes a :my lexical matches';
is $seen, 5, 'the write survives the lookahead';

my $m = "x  a" ~~ / 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> $n 'a' /;
is $m.Str, 'x  a', 'a :my lexical measured in a lookahead interpolates after it';
nok $m<sp>:exists, 'the lookahead keeps none of its captures';

ok "ab" ~~ / a <!before c> b /, 'a negated lookahead passes when its body fails';
nok "ac" ~~ / a <!before c> c /, 'and fails when its body matches';
ok "xab" ~~ / 'xa' <?after 'xa'> b /, 'a lookbehind tests what ends at the cursor';
nok "yab" ~~ / . a <?after 'xa'> b /, 'and fails when nothing ending there matches';

my $calls = 0;
"aaab" ~~ / <?before a+ { $calls++ } b> a+ b /;
is $calls, 1, 'a lookahead body is not resumed by a later backtrack';
