use Test;

plan 10;

# A statically-declared native typed array wraps out-of-range values on
# `.push`, matching C unsigned/wrapping semantics. This has always worked.
my uint8 @static-u8;
@static-u8.push(-1);
is @static-u8[0], 255, 'static my uint8 @a wraps -1 to 255 on push';

my int8 @static-i8;
@static-i8.push(300);
is @static-i8[0], 44, 'static my int8 @a wraps 300 to 44 on push';

# A native array built via a DYNAMIC (runtime-computed) `array[T]`
# parameterization must wrap the same way -- the element type is only known
# at runtime (`array[$cond ?? T1 !! T2]`), so a scalar-bound (`:=`) container
# whose live value lives only in the local slot until some later sync point
# (e.g. an I/O op) must still see the correct element-type metadata when
# `.push` wraps the value.
my $is-signed = False;
my $u8 := array[$is-signed ?? int8 !! uint8].new;
$u8.push(-1);
is $u8[0], 255, 'dynamically-parameterized array[uint8] wraps -1 to 255 on push';

my $is-signed2 = True;
my $i8 := array[$is-signed2 ?? int8 !! uint8].new;
$i8.push(300);
is $i8[0], 44, 'dynamically-parameterized array[int8] wraps 300 to 44 on push';

# uint16 / int16 width, to make sure the fix is not width-specific.
my $u16 := array[$is-signed ?? int16 !! uint16].new;
$u16.push(-1);
is $u16[0], 65535, 'dynamically-parameterized array[uint16] wraps -1 to 65535 on push';

my $i16 := array[$is-signed2 ?? int16 !! uint16].new;
$i16.push(70000);
is $i16[0], 4464, 'dynamically-parameterized array[int16] wraps 70000 to 4464 on push';

# Multiple pushed values in a single call all wrap.
my $multi := array[$is-signed ?? int8 !! uint8].new;
$multi.push(1, 300, -1);
is-deeply $multi.List, (1, 44, 255), 'multi-value push onto dynamic array[uint8] wraps each element';

# The element-type metadata itself must be readable before AND after the
# push (regression guard for the underlying dual-store staleness).
is $u8.of, uint8, 'array[uint8].of still reports uint8 after push';

# unshift/append/prepend go through the same wrap chokepoint as push.
my $u8b := array[$is-signed ?? int8 !! uint8].new;
$u8b.unshift(-1);
is $u8b[0], 255, 'unshift onto dynamic array[uint8] wraps -1 to 255';

my $u8c := array[$is-signed ?? int8 !! uint8].new(10);
$u8c.append(-1);
is $u8c[1], 255, 'append onto dynamic array[uint8] wraps -1 to 255';

done-testing;
