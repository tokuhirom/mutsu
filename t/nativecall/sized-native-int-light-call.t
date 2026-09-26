use Test;

# #9506: a routine with a sized native-int parameter (`uint32 $n`, `int8 $x`,
# ...) now takes the light call path. It must bind exactly what the general
# binder binds -- the same width wrap (see #9533 for how that differs from
# rakudo), the same accepted shapes and the same errors.

plan 14;

sub u32(uint32 $n) { $n }
sub u8(uint8 $n)   { $n }
sub i8(int8 $n)    { $n }
sub two(uint32 $n, $b) { $n + $b }

is u32(7), 7, 'an in-range Int binds unchanged';
is two(7, 5), 12, 'mixed with an untyped parameter';
is u32(2**33 + 1), 1, 'an out-of-range Int wraps to the width (as the general binder does)';
is u8(300), 44, 'uint8 wraps';
is i8(200), -56, 'int8 wraps to its signed range';
is u8(True), 1, 'a Bool unboxes to 1';
is u32(<42>), 42, 'an IntStr allomorph binds its Int half';

enum E <Zero One Two>;
ok u8(Two) == 2, 'an Int-valued enum is accepted';

my $sum = 0;
$sum += u8($_) for 250..260;
is $sum, (250..255).sum + (0..4).sum, 'repeated light calls wrap each argument';

sub introspect(uint32 $n) { $n ~~ uint32 }
ok introspect(5), 'the parameter still smartmatches its native type';

throws-like { u8("x") }, X::TypeCheck::Binding::Parameter,
    'a Str is rejected with the binder error';
like (try { u8("x") } // $!.message), /'expected uint8 but got Str'/,
    '... with the binder message';
throws-like { u8(Int) }, Exception, message => /'Cannot unbox a type object'/,
    'a type object is refused as the binder refuses it';
throws-like { u8(1.5) }, X::TypeCheck::Binding::Parameter, 'a Rat is rejected';
