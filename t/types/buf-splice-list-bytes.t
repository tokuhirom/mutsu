use v6;
use Test;

# Buf.splice with a plain list/array of replacement values must coerce each
# element to a byte, matching the scalar/Buf arms. A word-list of numeric
# strings (`<3 2 1>`) previously leaked Str values into the Buf, rendering
# as 0. (raku-doc/doc/Type/Buf.rakudoc)

plan 6;

my $b = Buf.new(1, 1, 2, 3, 5);
my $removed = $b.splice: 0, 3, <3 2 1>;
is $removed.raku, 'Buf.new(1,1,2)', 'splice returns the removed bytes';
is $b.raku, 'Buf.new(3,2,1,3,5)', 'numeric-string list replacement coerces to bytes';

my $c = Buf.new(1, 2, 3);
$c.splice: 1, 1, (9, 10);
is $c.raku, 'Buf.new(1,9,10,3)', 'Int list replacement works';

my $d = Buf.new(1, 2, 3);
$d.splice: 1, 0, Buf.new(7, 8);
is $d.raku, 'Buf.new(1,7,8,2,3)', 'Buf replacement (insert) works';

my $e = Buf.new(10, 20, 30);
$e.splice: 1, 2, <1 2>;
is $e.raku, 'Buf.new(10,1,2)', 'string-list replacement on the tail';

# Values > 255 wrap to a byte, like the scalar arm.
my $f = Buf.new(0);
$f.splice: 0, 1, (257,);
is $f.raku, 'Buf.new(1)', 'replacement value is masked to a byte';
