use Test;

# A bare listop whose argument list ends with a list-infix operator eats its own
# trailing whitespace, so the `->` that follows arrives glued to the term. That
# is a POINTY BLOCK, not the Perl 5 arrow: `->` is only the obsolete postfix when
# nothing separates it from the term. mutsu threw
# "Unsupported use of -> as postfix" for the whole statement instead.
# From Math::Vector's t/01-basics.rakutest (`for flat (...) X (...) -> $x, $y`).

plan 8;

my @pairs;
for flat (1, 2) X (3, 4) -> $x, $y {
    @pairs.push("$x-$y");
}
is @pairs.join(','), '1-3,1-4,2-3,2-4', 'for flat (...) X (...) -> $x, $y';

my @flat;
for flat (1, 2) X (3, 4) -> $v {
    @flat.push($v);
}
is @flat.join(','), '1,3,1,4,2,3,2,4', 'for flat (...) X (...) -> $v';

my @rev;
for reverse (1, 2) X (3, 4) -> $v {
    @rev.push($v.join('/'));
}
is @rev.join(','), '2/4,2/3,1/4,1/3', 'for reverse (...) X (...) -> $v';

my @z;
for flat (1, 2) Z (3, 4) -> $v {
    @z.push($v);
}
is @z.join(','), '1,3,2,4', 'the Z spelling keeps working too';

my @meta;
for flat (1, 2) X* (3, 4) -> $v {
    @meta.push($v);
}
is @meta.join(','), '3,4,6,8', 'a metaop over the cross operator';

# Without the listop, the whitespace is still in the remainder: unchanged.
my @plain;
for (1, 2) X (3, 4) -> $x, $y {
    @plain.push("$x$y");
}
is @plain.elems, 2, 'the parenthesized form is unaffected';

# The obsolete postfix arrow is still rejected: nothing separates it from `$x`.
my $x = 5;
throws-like 'my $y = 5; $y->foo', X::Obsolete, 'glued -> is still the Perl 5 arrow';

# ... and a pointy block after an ordinary term still parses.
my $c = -> $a { $a * 2 };
is $c(4), 8, 'a bare pointy block still parses';
