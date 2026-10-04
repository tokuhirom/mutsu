use Test;

plan 4;

# An `is rw` scalar param is not readonly just because an enclosing block has
# a sigilless `\x` of the same bare name (#11700).
my @out;
lives-ok {
    (1, 2).map(-> \x { my @a = 1; @a.map(-> $x is rw { $x *= 10 }).eager }).eager
}, 'is rw $x inside a live \x block can be assigned';

my @src = 1, 2, 3;
(1,).map(-> \x { @src.map(-> $x is rw { $x *= 10 }).eager }).eager;
is-deeply @src, [10, 20, 30], 'the rw param writes back to the source';

# The enclosing \x is still readonly afterwards.
(1,).map(-> \x { @src.map(-> $x is rw { $x += 1 }).eager; @out.push(x) }).eager;
is-deeply @out, [1], 'the enclosing \x still reads its own value';

# A compound assignment to a readonly parameter fails like plain assignment.
throws-like { sub f($x) { { $x ~= "a" }() }; f("b") }, X::AdHoc,
    message => /'readonly'/,
    'compound assignment to a readonly param names assignment, not postfix:<++>';

done-testing;
