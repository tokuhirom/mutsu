use Test;

# A typed or defaulted array answers a present element as itself, and a miss
# (a hole, past the end, a stored Nil) with its element type or default --
# through `.AT-POS` and through the subscript alike.

plan 12;

my Int @i = 1, 2, 3;
is @i.AT-POS(1), 2, 'a present element of a typed array';
is @i[2], 3, 'the same through the subscript';
is @i.AT-POS(7).raku, 'Int', 'past the end is the element type';
@i[5] = 9;
is @i.AT-POS(4).raku, 'Int', 'a hole is the element type';
is @i[4].raku, 'Int', 'the same through the subscript';

my @d is default(42) = 1, 2;
is @d.AT-POS(0), 1, 'a present element of a defaulted array';
is @d.AT-POS(9), 42, 'past the end is the default';
@d[1] = Nil;
is @d.AT-POS(1), 42, 'a stored Nil reads as the default';

my Any @t = Int, Str;
is @t.AT-POS(1).raku, 'Str', 'a stored type object reads back as itself';

my Str @s;
@s[2] = 'c';
is @s.AT-POS(0).raku, 'Str', 'a hole in a Str array';

my $x = 5;
my Int @b;
@b[0] := $x;
$x = 6;
is @b.AT-POS(0), 6, 'a bound element follows its container';

sub scan(\h) { (^h.elems).map({ h.AT-POS($_).raku }).join(',') }
is scan(@i), '1,2,3,Int,Int,9', 'through a sigilless parameter';
