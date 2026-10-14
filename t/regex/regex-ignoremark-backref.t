use Test;

plan 17;

# Under `:m` (ignoremark) rakudo compares a backreference exactly, marks
# included, as `:i` does not fold one either; only the pattern's own atoms
# compare bases (#13201). Every expectation was checked against rakudo.

sub m($r) { $r ?? ~$r !! Nil }

nok "a\x[301]a" ~~ /:m (a) $0/, 'a marked capture does not repeat as a bare base';
nok "aa\x[301]" ~~ /:m (a) $0/, 'a bare capture does not repeat as a marked base';
nok "a\x[301]a" ~~ /:i :m (A) $0/, '... nor under :i :m';
is m("a\x[301]a\x[301]" ~~ /:m (a) $0/), "a\x[301]a\x[301]", 'the same marks repeat';
is m("xa\x[301]a\x[301]" ~~ /:m (a) $0/), "a\x[301]a\x[301]", '... after an unmarked prefix';
is m("a\x[301]a a\x[301]a\x[301]" ~~ /:m (a) $0/), "a\x[301]a\x[301]", '... found past an earlier near miss';
is m("a\x[301]a\x[301]" ~~ /:m $<k>=(a) $<k>/), "a\x[301]a\x[301]", 'a named backreference with the same marks';
nok "a\x[301]a" ~~ /:m $<k>=(a) $<k>/, 'a named backreference does not ignore marks';
nok "ba\x[301]a" ~~ /b [:m (a) $0]/, 'a scoped [:m] group compares its backreference exactly';
is m("ba\x[301]a\x[301]" ~~ /b [:m (a) $0]/), "ba\x[301]a\x[301]", '... and matches the same marks';
is m("a\x[301]e\x[300]a\x[301]e\x[300]" ~~ /:m (a.) $0/), "a\x[301]e\x[300]a\x[301]e\x[300]",
    'a capture spanning two marked graphemes';
nok "a\x[301]e\x[300]a\x[301]e" ~~ /:m (a.) $0/, '... does not repeat with its last mark missing';
is m("a\x[301]a\x[301]a\x[301]" ~~ /:m (a)+ $0/), "a\x[301]a\x[301]", 'a quantified capture';

# The literal atoms still compare bases.
is m("e\x[301]b" ~~ /:m (e) b/), "e\x[301]b", 'a literal atom ignores marks';

grammar G { token TOP { :m (a) $0 } }
nok G.parse("a\x[301]a"), 'a :m grammar token compares a backreference exactly';
ok G.parse("a\x[301]a\x[301]"), '... and matches the same marks';
is ~G.parse("a\x[301]a\x[301]")[0], "a\x[301]", '... capturing the marked grapheme';
