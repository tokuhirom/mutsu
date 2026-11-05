use Test;

# A whole-array assignment to a captured fixed-dimension array from a nested
# sub or closure keeps the shape (padding, bounds, element type) instead of
# replacing the array by a plain one (#16604). Expectations measured against
# rakudo 2026.09.

plan 14;

my @e[3];
sub fill2 { @e = 1, 2 }
fill2();
is @e.raku, 'Array.new(:shape(3,), [1, 2, Any])', 'fewer items pad the shape';
is @e.shape.raku, '(3,)', 'and the shape is kept';

sub clear { @e = () }
clear();
is @e.raku, 'Array.new(:shape(3,), [Any, Any, Any])', 'an empty list clears it';

sub over { @e = 1, 2, 3, 4 }
try over();
is $!.message, 'Index 3 for dimension 1 out of range (must be 0..2)', 'too many items die';
is @e.shape.raku, '(3,)', 'and the shape survives the failure';

sub words { @e = <a b c> }
words();
is @e.raku, 'Array.new(:shape(3,), ["a", "b", "c"])', 'an exact fit';

my @n[2] of Int;
sub ints { @n = 1, 2 }
ints();
is @n.raku, 'Array[Int].new(:shape(2,), [1, 2])', 'the element type is kept';
is @n.shape.raku, '(2,)', 'with its shape';

my @w[2;2];
sub rows { @w = (1, 2), (3, 4) }
rows();
is @w.raku, 'Array.new(:shape(2, 2), [1, 2], [3, 4])', 'a two-dimensional array takes rows';
is @w.shape.raku, '(2, 2)', 'and keeps both dimensions';

my @p[2] = 5, 6;
my &c = { @p = 7, 8 };
c();
is @p.raku, 'Array.new(:shape(2,), [7, 8])', 'a closure assigns too';

my @plain = 1, 2;
sub grow { @plain = 1, 2, 3 }
grow();
is @plain.raku, '[1, 2, 3]', 'a plain array still takes any number of items';

my @flat[2;2];
sub flat-rows { @flat = 1, 2, 3, 4 }
try flat-rows();
is $!.^name, 'X::Assignment::ToShaped', 'a flat list for a two-dimensional array is refused';
is @flat.shape.raku, '(2, 2)', 'and the shape survives';
