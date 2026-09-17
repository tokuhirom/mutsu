use Test;

# `inject_implicit_rule_ws` turns significant whitespace right before a
# closing `]`/`)` into an implicit `<.ws>` (see
# rule-sigspace-trailing-whitespace.t for the "last atom" case). When the
# group's content is a top-level `|`/`||` alternation, that `<.ws>` used to
# land textually only inside whichever branch happened to sit next to the
# close — so a quantifier on the group (`[ A || B ]+`) could skip the
# separator after the LAST-written branch matched, but not after any other
# one. Real-world fallout: mutsu#8561, where Ujumla's
# `[ <.comment> || <config-line> || <config-section> || <include> ||
# <.empty-or-blank> ]+` never advanced past a leading `<.comment>` match,
# because the implicit `<.ws>` was attached only to `<.empty-or-blank>`.

plan 4;

grammar G1 {
    rule TOP { ^ [ 'a' | 'b' ]+ $ }
}
ok G1.parse("a b a b b a"), 'quantified alternation with a single `|` skips whitespace after every branch';

grammar G2 {
    rule TOP { ^ [ 'a' || 'bb' ]+ $ }
}
ok G2.parse("a bb a a bb"), 'quantified LTM alternation (`||`) skips whitespace after every branch';

# The Ujumla shape: the FIRST alternative in source order is the one that
# must match repeatedly, with a later alternative in the same group.
grammar G3 {
    rule TOP { [ <.comment> || <line> ]+ }
    regex comment { \h* '#' \N* }
    token name { \S+ }
    rule line { <name> }
}
{
    my $r = G3.parse("# a comment\nfirst\nsecond\n");
    ok $r, 'a leading comment branch still lets a later iteration match a different branch';
    is $r ?? $r<line>.elems !! -1, 2, 'both non-comment lines are captured as separate <line> iterations';
}

done-testing;
