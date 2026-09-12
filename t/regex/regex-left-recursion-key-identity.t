use v6;
use Test;

# The regex engine's left-recursion bookkeeping is keyed by
# `(rule name, argument identity, characters remaining)`. Round 14 of #7576
# replaced three `String`-keyed thread-local tables with a single map whose key
# holds the *interned* rule name and the argument identity in separate fields,
# instead of joining them into one string with a NUL separator; an activation
# now also has to erase itself from that one map on the way out.
#
# These rows pin the three ways the key has to discriminate, plus the teardown:
# a key that conflated two calls would make the second read the first's empty
# seed and fail, and a retained entry would do the same to a later call at the
# same position.
#
# The left-recursive rows have no `raku` oracle -- Rakudo has no growing-seed
# loop and hangs on a left-recursive rule (same caveat as
# `t/regex/regex-lazy-candidate-enumeration.t`'s E3d). Every other row matches
# raku.

plan 7;

# The seed-growing loop itself: `expr` re-enters itself at the same position.
grammar LeftRec {
    token TOP  { <expr> }
    token expr { <expr> '+' <term> | <term> }
    token term { \d+ }
}
is ~(LeftRec.parse('1+2+3') // ''), '1+2+3', 'a left-recursive rule grows its seed';
is ~(LeftRec.parse('7') // ''), '7', 'and still matches its base case';

# Name discrimination: `item` and `item-list` are distinct keys at the same
# position, even though one name is a prefix of the other.
grammar PrefixNames {
    token TOP       { <item-list> }
    token item-list { <item>+ % ',' }
    token item      { \w+ }
}
is ~(PrefixNames.parse('a,b,c') // ''), 'a,b,c',
    'rule names that share a prefix are separate activations';

# Argument discrimination: `<depth($n-1)>` at the same position is ordinary
# recursion toward a base case, not left recursion, so it must not read the
# outer call's seed.
grammar Parameterized {
    rule TOP { <depth(3)> }
    multi rule depth(0)  { \d+ }
    multi rule depth($n) { <depth($n-1)> }
}
ok Parameterized.parse('42').defined,
    'the same rule at the same position with a different argument is not left recursion';

# Teardown: two sibling calls to the same rule at the same zero-width position.
# The second one must not find the first one's activation still registered.
grammar Siblings {
    token gap { \s* }
    token TOP { 'a' <.gap> <.gap> 'b' }
}
ok Siblings.parse('ab').defined, 'an activation is erased when its call returns';

# ... and the same rule reached again at a *later* position, after the first
# activation at an earlier one has ended.
grammar Repeated {
    token word { \w+ }
    token TOP  { <word> ' ' <word> }
}
is ~(Repeated.parse('one two') // ''), 'one two',
    'the same rule at two positions keeps two independent keys';

# The key carries no package, so two grammars that define the same rule name
# share it -- which is sound only because an activation never outlives its call.
grammar SameNameA { token TOP { <part> }; token part { 'x' } }
grammar SameNameB { token TOP { <part> }; token part { 'y' } }
ok SameNameA.parse('x').defined && SameNameB.parse('y').defined,
    'a rule name shared by two grammars resolves per grammar';
