use Test;

plan 11;

# Under sigspace, a quantifier that significant whitespace follows is not
# ratcheted: Rakudo ratchets the `[atom <.ws>]` wrapper, so the quantifier
# gives an element back when what follows needs it (ASN::Grammar's
# `'DEFAULT' <id-string>? <value>` on "DEFAULT FALSE").
grammar G {
    rule opt  { 'D' <id>? <v> }
    rule star { 'D' \w* <v> }
    rule lit  { 'D' x? x }
    rule grp  { 'D' [x|y]? x }
    rule rng  { 'D' <id> ** 0..1 <v> }
    rule commit { 'D' <id>?: <v> }
    token tight { :s 'D' <id>?<v> }
    token inline { :s 'D' <id>? <v> }
    token id { <[a..z]>+ }
    token v  { <[a..z]>+ }
}
ok G.subparse('D x', :rule<opt>), '<id>? gives back to <v>';
is ~G.subparse('D x', :rule<opt>)<v>, 'x', '... and <v> got it';
ok G.subparse('D x', :rule<star>), '\w* backtracks';
ok G.subparse('D x', :rule<lit>), 'a literal x? backtracks';
ok G.subparse('D x', :rule<grp>), 'a quantified group backtracks';
ok G.subparse('D x', :rule<rng>), 'a ** range backtracks';
nok G.subparse('D x', :rule<commit>), 'an explicit : still commits';
nok G.subparse('D x', :rule<tight>), 'no whitespace after the quantifier: ratcheted';
ok G.subparse('D x', :rule<inline>), 'inline :s behaves like rule';

# The whitespace between `**` and its count is part of the quantifier.
ok 'aaa b' ~~ rule { a ** 1..3 b }, 'a ** 1..3 under sigspace';
nok 'aaab' ~~ rule { a ** 1..3 b }, '... still needs the <.ws> after the count';
