use lib 't/lib';
use Test;
use NativeAliasConst;

# `my MyByte $x;` where `MyByte` is an imported constant alias of a native
# type is a native slot defaulting to 0 (Font::FreeType's `my FT_Bool $is-cid`).

plan 4;

my MyByte $v;
is $v, 0, 'defaults to 0';
sub bump(MyByte $x is rw) { $x = 3 }
my MyByte $w;
bump($w);
is $w, 3, 'passed as an is rw argument';
my MyByte $z = 5;
is $z, 5, 'an explicit initializer still works';
is $v + 1, 1, 'usable as a number';
