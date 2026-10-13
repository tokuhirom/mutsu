use Test;

# #13333: an enum member's `.ACCEPTS` is its base type's, comparing by value
# as the smartmatch operator does (`Less.ACCEPTS(-1)` is `Int`'s, `True`);
# it compared enum identity only.

plan 13;

enum C (x => "s", y => "t");
enum N <a b c>;

ok Less.ACCEPTS(-1), 'Order::Less accepts its value';
ok Less.ACCEPTS("-1"), 'and a numeric string';
ok Less.ACCEPTS(Less), 'and itself';
nok Less.ACCEPTS(More), 'not another member';
nok Less.ACCEPTS(Order), 'not the enum type object';
ok b.ACCEPTS(1), 'an Int-valued member accepts its value';
ok b.ACCEPTS(1.0), 'numerically';
nok b.ACCEPTS(N::c), 'not another member';
ok x.ACCEPTS("s"), 'a Str-valued member accepts its string';
nok x.ACCEPTS("t"), 'not another string';
nok x.ACCEPTS(y), 'not another member';
is-deeply Less.ACCEPTS(-1), (-1 ~~ Less), 'agrees with the smartmatch operator';
ok Bool::True.ACCEPTS(False), 'Bool is still its own';
