use v6;
use Test;

# Under :ratchet, rakudo ratchets each term of a sequence on its outermost
# node. A term that significant (sigspace) whitespace follows is wrapped as
# `[term <.ws>]`, and the ratchet lands on that wrapper, so the term itself
# stays backtrackable. Every expected value below is rakudo 2026.09's.

plan 26;

# A ratcheted `||` commits to its first matching branch, zero-width or not
# (#11162).
grammar SeqAltToken { token TOP { '(' [ <literal>? || <fallback> ] ')' }; token literal { 'x' }; token fallback { '@' \d+ } }
nok SeqAltToken.subparse('(@42)'), 'token: a zero-width first || branch commits';
grammar SeqAltNoWs { rule TOP {'('[<literal>?||<fallback>]')'}; token literal { 'x' }; token fallback { '@' \d+ } }
nok SeqAltNoWs.subparse('(@42)'), 'rule without whitespace after ]: the || commits';
grammar SeqAltExplicitWs { token TOP { '(' [ <literal>? <.ws> || <fallback> <.ws> ] ')' }; token literal { 'x' }; token fallback { '@' \d+ } }
nok SeqAltExplicitWs.subparse('(@42)'), 'an explicit <.ws> inside the branches does not un-ratchet the ||';

grammar SeqAltRule { rule TOP { '(' [<literal>?||<fallback>] ')' }; token literal { 'x' }; token fallback { '@' \d+ } }
my $m = SeqAltRule.subparse('(@42)');
ok $m, 'rule: whitespace after ] lets the || move on to its next branch';
is ~$m<fallback>, '@42', '... and the next branch captures';
grammar SeqAltInlineS { token TOP { :s '(' [<literal>?||<fallback>] ')' }; token literal { 'x' }; token fallback { '@' \d+ } }
ok SeqAltInlineS.subparse('(@42)'), 'token with :s: same as a rule';

ok so("!!?" ~~ m:r:s/ [ '!' || '!!' ] '?' /), 'm:r:s, consuming first branch, whitespace after ]';
grammar SeqAltConsuming { rule TOP { [ '!' || '!!' ] '?' } }
ok SeqAltConsuming.parse('!!?'), 'rule: backtracks into a consuming || branch';
grammar SeqAltConsumingNoWs { rule TOP { [ '!' || '!!' ]'?' } }
nok SeqAltConsumingNoWs.parse('!!?'), 'rule: no whitespace after ], the || commits';

# The same holds for every term that can backtrack.
grammar SubruleWs { rule TOP { <b> '!' }; regex b { <[x!]>+ } }
ok SubruleWs.parse('x!!'), 'rule: a subrule call followed by whitespace is backtrackable';
grammar SubruleNoWs { rule TOP { <b>'!' }; regex b { <[x!]>+ } }
nok SubruleNoWs.parse('x!!'), 'rule: a subrule call without whitespace after it commits';
grammar SubruleInlineS { token TOP { :s <b> '!' }; regex b { <[x!]>+ } }
ok SubruleInlineS.parse('x!!'), 'token :s: a subrule call followed by whitespace is backtrackable';
grammar SubruleNewline { rule TOP { <b>
    '!' }; regex b { <[x!]>+ } }
ok SubruleNewline.parse('x!!'), 'a newline is significant whitespace too';
grammar AngleAlias { rule TOP { <x=b> '!' }; regex b { <[x!]>+ } }
ok AngleAlias.parse('x!!'), 'an angle alias <x=b> is still the call itself';

grammar QuantWs { rule TOP { <[x!]>+ '!' } }
ok QuantWs.parse('x!!'), 'rule: a quantifier followed by whitespace is backtrackable';
grammar QuantNoWs { rule TOP { <[x!]>+'!' } }
nok QuantNoWs.parse('x!!'), 'rule: a quantifier without whitespace after it commits';

grammar LtmWs { rule TOP { [ '!' | '!!' ] '!' } }
ok LtmWs.parse('!!'), 'rule: an LTM | followed by whitespace is backtrackable';
grammar LtmNoWs { rule TOP { [ '!' | '!!' ]'!' } }
nok LtmNoWs.parse('!!'), 'rule: an LTM | without whitespace after it commits';

grammar CaptureWs { rule TOP { ( '!' || '!!' ) '?' } }
ok CaptureWs.parse('!!?'), 'rule: a capture group followed by whitespace is backtrackable';
grammar CaptureNoWs { rule TOP { ( '!' || '!!' )'?' } }
nok CaptureNoWs.parse('!!?'), 'rule: a capture group without whitespace after it commits';

# A [ ] group around one term is that term.
grammar GroupOne { rule TOP { [<b>] '!' }; regex b { <[x!]>+ } }
ok GroupOne.parse('x!!'), 'rule: [<b>] followed by whitespace un-ratchets <b>';
grammar GroupOneNoWs { rule TOP { [<b>]'!' }; regex b { <[x!]>+ } }
nok GroupOneNoWs.parse('x!!'), 'rule: [<b>] without whitespace after it commits';
grammar GroupOneExplicit { token TOP { [<b>]:! '!' }; regex b { <[x!]>+ } }
ok GroupOneExplicit.parse('x!!'), 'token: [<b>]:! un-ratchets <b>';

# A sigil alias ratchets the atom it binds itself.
grammar SigilAliasQuant { rule TOP { $<x>=<[x!]>+ '!' } }
nok SigilAliasQuant.parse('x!!'), 'rule: $<x>=quantifier stays ratcheted';
grammar SigilAliasSubrule { rule TOP { $<x>=<b> '!' }; regex b { <[x!]>+ } }
nok SigilAliasSubrule.parse('x!!'), 'rule: $<x>=<subrule> stays ratcheted';
grammar SigilAliasGroup { rule TOP { $<a>=[ '!' || '!!' ] '?' } }
nok SigilAliasGroup.parse('!!?'), 'rule: $<a>=[ || ] stays ratcheted';
