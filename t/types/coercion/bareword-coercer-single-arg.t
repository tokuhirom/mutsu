use Test;

# A bareword coercer called with ONE argument coerces it like the method form
# (`List(x)` is `x.List`, `Int(x)` is `x.Int`), and returns a value already of
# the target type unchanged. Several arguments keep one element each (#11299).

plan 19;

is-deeply List((1, 2)), (1, 2), 'List of a List contributes its elements';
is-deeply List($(1, 2)), (1, 2), 'List of an itemized List';
is-deeply List([1, 2]), [1, 2], 'List of an Array returns the Array itself';
is-deeply Array((1, 2)), [1, 2], 'Array of a List';
is-deeply Array((1, 2).Seq), [1, 2], 'Array of a Seq';
is-deeply Array(1..3), [1, 2, 3], 'Array of a Range';
is-deeply List(5), (5,), 'List of a scalar';
is-deeply List(1, 2), (1, 2), 'several arguments, one element each';
is-deeply Array(1, (2, 3)), [1, (2, 3)], 'several arguments are not flattened';

is Int((1, 2)), 2, 'Int of a List is its element count';
is Int((1, 2).Seq), 2, 'Int of a Seq is its element count';
is Num((1, 2, 3)), 3e0, 'Num of a List is its element count';
is-deeply Int(True), True, 'Int of a Bool returns the Bool (already an Int)';
is-deeply Int(3.7), 3, 'Int of a Rat truncates';
cmp-ok Num(0.7777777777777777777771), '==', Num(0.777777777777777777777),
    'Num of a big Rat is correctly rounded';
isa-ok Int("x"), Failure, 'Int of a non-numeric Str is a Failure';
throws-like { Int(1+2i) }, X::Numeric::Real, 'Int of a non-real Complex throws';

class C { method Int { 7 } }
is Int(C.new), 7, 'Int calls a user class .Int';
is Array(Int).^name, 'Array(Int)', 'a lone type object is still a coercion type';
