use Test;

# The `:sym<...>` proto-candidate adverb is word-quoted, so surrounding
# whitespace inside the angle brackets is insignificant: a grammar candidate
# `token c:sym<foo >` names the same candidate as its action method
# `method c:sym<foo>`. Before this fix mutsu kept the stray space in the parsed
# candidate name, so the action never fired and `.made` was undefined.
# (POFile: `token comment:sym<format-directive >` vs
# `method comment:sym<format-directive>`.)

plan 5;

# Token-side whitespace (leading and trailing) must dispatch to the trimmed
# action methods. This mirrors POFile's grammar exactly.
grammar G {
    proto token c { * }
    token c:sym<a > { 'AA' }          # trailing space
    token c:sym< b > { 'BB' }         # leading + trailing space
    token c:sym<c>  { 'CC' }          # no space (baseline)
    token TOP { <c>+ }
}
class Act {
    method c:sym<a>($/) { make 'A' }
    method c:sym<b>($/) { make 'B' }
    method c:sym<c>($/) { make 'C' }
    method TOP($/) { make $<c>>>.made.join }
}
is G.parse('AABBCC', actions => Act).made, 'ABC',
    'token sym<> candidates with whitespace dispatch to their trimmed actions';

# Whitespace on the method side normalizes the same way.
grammar G2 {
    proto token c { * }
    token c:sym<x> { 'XX' }
    token c:sym<y> { 'YY' }
    token TOP { <c>+ }
}
class Act2 {
    method c:sym<x >($/) { make 'X' }   # space on the method side
    method c:sym<y>($/)  { make 'Y' }
    method TOP($/) { make $<c>>>.made.join }
}
is G2.parse('XXYY', actions => Act2).made, 'XY',
    'whitespace on the method-side sym also normalizes';

# A trimmed candidate still parses its own `<sym>` (the stored candidate
# identity is not corrupted by the trim).
grammar G3 {
    proto token kw { * }
    token kw:sym<if > { <sym> }
    token kw:sym<else> { <sym> }
    token TOP { <kw> }
}
is G3.parse('if').<kw>.Str, 'if', 'a trimmed sym candidate still parses its <sym>';

# Ordinary operator sym names and multi dispatch are unaffected.
sub infix:<plus>($a, $b) { $a + $b }
is (2 [plus] 3), 5, 'ordinary infix sym operator still works';

multi mysub(Int $x) { "int:$x" }
is mysub(3), 'int:3', 'ordinary multi dispatch still works';
