use Test;

# `$m[$i]` on a lazy Match (a regex match or a grammar cursor) is answered by
# a fast arm of the subscript (#18416). These pin what it answers against the
# general path's semantics.

plan 17;

# A plain regex match.
my $m = "ab12" ~~ / (<[a..z]>+) (\d+) /;
is ~$m[0], 'ab', 'first positional capture';
is ~$m[1], '12', 'second positional capture';
ok $m[2] === Nil, 'an index past the captures is Nil';
ok $m[0] === $m[0], 'a repeated read hands out the same capture';
is $m[*-1].Str, '12', 'a WhateverCode index still works';
is $m[0..1].map(~*).join(','), 'ab,12', 'a Range slice still works';

# An unmatched optional capture.
my $o = "x" ~~ / (y)? (x) /;
ok $o[0] === Nil, 'an unmatched optional capture is Nil';
is ~$o[1], 'x', 'the capture after it';

# A quantified group is an Array, and the same Array on every read.
my $q = "a/b/c" ~~ / [ (<[a..z]>) ]+ % '/' /;
is $q[0].map(~*).join(','), 'a,b,c', 'a quantified group reads as its Array';
$q[0].push('extra');
is $q[0].elems, 4, 'a change to the quantified Array is kept';

# A grammar cursor, read inside an action method and after the parse.
grammar G {
    token TOP { [ (<[a..z]>+) ]+ % '/' }
}
class A {
    has @.seen;
    method TOP($/) { @!seen.push: ~$/[0][1]; make $/[0].elems }
}
my $actions = A.new;
my $p = G.parse("page/abc", :$actions);
is $p.made, 2, 'an action reads its cursor\'s positional capture';
is $actions.seen.join, 'abc', 'the nested element read in the action';
is $p[0].map(~*).join(','), 'page,abc', 'the cursor\'s capture after the parse';
is $p[0][0].^name, 'G', 'a grammar capture reports the grammar class';

# A user postcircumfix:<[ ]> for Match still wins.
{
    multi sub postcircumfix:<[ ]>(Match $m, Int $i) { "user:$i" }
    my $u = "q" ~~ / (q) /;
    is $u[0], 'user:0', 'a user postcircumfix candidate answers';
}

# A grammar whose class does Sequence with its own iterator is subscripted
# through its elements.
grammar S does Sequence {
    token TOP { (x) }
    method iterator() { <s0 s1>.iterator }
}
my $s = S.parse("x");
is $s[1], 's1', 'a Sequence cursor is subscripted through its iterator';
is $s.list.elems, 2, 'and listed through it';
