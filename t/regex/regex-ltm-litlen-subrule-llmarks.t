use Test;

# The longest-literal (`litlen`) tie-break between `|` branches whose
# declarative prefixes are equally long. Rakudo marks a literal as counting
# toward it while building each rule's own NFA (NQP's `_LL` edges), so a
# subrule's leading literals count wherever the call sits — also inside a
# quantified group — while a literal written inside a quantifier, or after a
# subrule call, does not.
#
# Found in ANTLR4::Grammar 0.6.3, t/10-basic-grammar.t: its
# `LEXER_CHAR_SET_RANGE` (`[<ELEM-NO-HYPHEN> '-']? <ELEM>`) lost to the bare
# `<LEXER_CHAR_SET_ELEMENT>` on `[\u000a-\u000c]`, because the escape's
# `<HEX_DIGIT> ** {4}` ends both prefixes at the same place and mutsu gave the
# range branch no litlen at all.

plan 7;

grammar Range {
    token TOP   { '[' (<range> | <elem>)* ']' }
    token hex   { <[ 0..9 a..f A..F ]> }
    token uesc  { 'u' <hex> ** {4} }
    token elem  { '\\' <-[ u ]> | '\\' <uesc> | <-[ \\ \x[5d] ]> }
    token nohy  { '\\' <-[ u ]> | '\\' <uesc> | <-[ - \\ \x[5d] ]> }
    token range { [<nohy> '-']? <elem> }
}

my $m = Range.parse(Q/[\u000a-\u000c]/);
ok $m, 'the ANTLR-style character set parses';
is $m[0].elems, 1, 'as one element';
ok $m[0][0]<range><nohy>, 'which is a range, not a lone escape';

grammar G {
    token u   { 'u' <[0..9]> ** {4} }
    token e   { 'x' <u> }
    token r   { [<e> '-']? <e> }
    token rq  { [ 'x' 'u' <[0..9]> ** {4} '-' ]? 'q' }
    token t1  { <r> | <e> }
    token t2  { <e> | <r> }
    token t3  { <rq> | <e> }
}

# Both prefixes end at the `** {4}` fate after "xu", and both branches reach
# the callee's literals "xu": equal litlen, so declaration order decides.
is G.subparse('xu0001-xu0002', :rule<t1>).Str, 'xu0001-xu0002',
    'a quantified call still counts its own leading literals';
is G.subparse('xu0001-xu0002', :rule<t2>).Str, 'xu0001',
    'on a full tie the earlier branch wins';

# Literals written inside the quantified group do not count: the bare call's
# "xu" outranks them.
is G.subparse('xu0001-xu0002', :rule<t3>).Str, 'xu0001',
    'a literal inside a quantifier does not count';

grammar H {
    token a  { 'a' }
    token p1 { <a> 'bc' \w }
    token p2 { 'ab' \w\w }
    token t  { <p1> | <p2> }
}

# Both branches match all of "abcd". `<a> 'bc'` has litlen 1 (only the
# callee's 'a'), `'ab'` has 2.
ok H.subparse('abcd', :rule<t>)<p2>,
    'a literal after a subrule call does not extend litlen';
