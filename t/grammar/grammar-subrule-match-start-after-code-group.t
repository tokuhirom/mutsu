use Test;

# A subrule is a regex of its own: `$/` inside its code starts at the subrule,
# wherever the caller is. A group in the caller that holds code shares the
# caller's scope, and the walk used to leave that scope published while the
# rest of the caller ran, so a subrule called after the group saw the caller's
# match start (`$/` spanned `abc12`). Expected values are raku's.

plan 6;

my @seen;

grammar AfterBlockGroup {
    token TOP { 'ab' [ 'c' { } | 'q' ] <y> }
    token y { \d+ <?{ @seen.push(~$/); True }> }
}
ok AfterBlockGroup.parse("abc12"), 'a subrule after a group with a block parses';
is @seen.join('|'), '12', '$/ in the subrule starts at the subrule';

@seen = ();
grammar AfterAssertionGroup {
    token TOP { 'ab' [ <x> <?{ True }> | 'q' ] <y> }
    token x { 'c' }
    token y { \d+ <?{ @seen.push(~$/); True }> }
}
ok AfterAssertionGroup.parse("abc12"), 'a subrule after a group with an assertion and a subrule parses';
is @seen.join('|'), '12', '$/ in the later subrule starts at that subrule';

@seen = ();
grammar NoCodeGroup {
    token TOP { 'ab' [ <x> | 'q' ] <y> }
    token x { 'c' }
    token y { \d+ <?{ @seen.push(~$/); True }> }
}
ok NoCodeGroup.parse("abc12"), 'the same shape without code in the group parses';
is @seen.join('|'), '12', 'and gives the same `$/`';
