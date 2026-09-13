use v6;
use Test;

# A look-behind is answered by running its pattern forward from candidate start
# positions and asking whether one ends at the current position. The engine used
# to try every start from 0, which made one `<?after …>` cost O(pos) and any
# pattern that evaluates one per input line quadratic (#7576). It now starts no
# earlier than `pos` minus the most the look-behind pattern can consume.
#
# That bound is only sound if it is an upper bound, so these rows exercise the
# shapes where getting it wrong changes an answer: a pattern the engine must NOT
# bound (an unbounded quantifier), one whose grapheme cluster is longer than one
# `char` (a base plus combining marks), the alternation/group cases where the
# bound is the longest branch, and position 0 where the floor has to clamp.
# Every expectation below was verified against rakudo v2026.07.
#
# The CRLF cluster is deliberately not exercised from Raku: a `\r\n` written in
# a *pattern* cannot consume the single cluster as two atoms in either engine, so
# the rows one can write here say nothing about the look-behind. The floor's CRLF
# handling is pinned by `regex_lookbehind`'s own unit tests instead, and the
# bound declines to model `\n` at all, so such a look-behind keeps searching from
# 0 exactly as before.

plan 19;

# --- the bounded single-atom case: what the fix actually speeds up -----------
ok 'abc' ~~ / 'ab' <?after b> c /, 'positive look-behind of one class matches';
nok 'abc' ~~ / 'ab' <!after b> c /, 'negative look-behind of one class rejects';
ok 'abc' ~~ / 'ab' <!after z> c /, 'negative look-behind passes when it cannot match';
ok 'a1c' ~~ / 'a1' <?after \d> c /, 'one-class look-behind over a digit';

# --- a multi-atom bound is the sum, and must look back far enough ------------
ok 'xabc' ~~ / 'xab' <?after 'ab'> c /, 'two-literal look-behind looks back two';
nok 'xabc' ~~ / 'xab' <?after 'xa'> c /, 'two-literal look-behind does not match further back';
ok 'xabc' ~~ / 'xab' <?after x? 'ab'> c /, 'an optional atom widens the bound';

# --- a bound the engine must not apply: an unbounded quantifier --------------
ok 'aaaab' ~~ / 'aaaa' <?after a+> b /, 'unbounded look-behind still searches back';
ok 'aaaab' ~~ / 'aaaa' <?after ^ a+> b /, 'anchored unbounded look-behind still matches';
nok 'aaaab' ~~ / 'aaaa' <!after a+> b /, 'negated unbounded look-behind still rejects';

# --- alternation / group: the bound is the longest branch --------------------
ok 'xyabc' ~~ / 'xyab' <?after [ 'ab' | 'zzzz' ]> c /, 'alternation look-behind takes a branch';
ok 'xyabc' ~~ / 'xyab' <?after [ 'yab' ]> c /, 'group look-behind looks back three';
nok 'xyabc' ~~ / 'xyab' <?after [ 'qq' | 'zz' ]> c /, 'alternation look-behind that cannot match';

# --- clusters are not chars: a base plus combining marks is one grapheme -----
# "e" + COMBINING ACUTE + COMBINING CIRCUMFLEX is a single cluster, so a
# one-cluster look-behind has to step back over all three chars.
my $combining = "a\c[LATIN SMALL LETTER E]\c[COMBINING ACUTE ACCENT]\c[COMBINING CIRCUMFLEX ACCENT]z";
ok $combining ~~ / ^ . <-[z]>+ <?after \w> z $ /,
    'one-cluster look-behind steps back over combining marks';
ok $combining ~~ / ^ . <-[z]>+ <!after \d> z $ /,
    'negated one-cluster look-behind over combining marks';

# --- the floor has to clamp at the start of the string ----------------------
ok 'ab' ~~ / ^ <!after \w> 'ab' $ /, 'negated look-behind at position 0 passes';
nok 'ab' ~~ / ^ <?after \w> 'ab' $ /, 'positive look-behind at position 0 fails';

# --- a look-behind inside a quantified group, the YAMLish shape -------------
# `token block-ws($indent) { <.space>* [ <!after <.alnum>> <.comment> … ]* }` is
# what made this quadratic; the essential shape is a per-iteration look-behind.
ok "a   # c\nb" ~~ / ^ a \s* [ <!after \w> '#' \N* ]? \n b $ /,
    'look-behind inside an optional group, the block-ws shape';
ok "ax  # c\nb" ~~ / ^ 'ax' \s* [ <!after \d> '#' \N* ]? \n b $ /,
    'the same shape with the look-behind passing on a letter';
