use Test;

# Stores into sized native int lexicals take a lane that skips the generic
# type match (#15079); the overflow wrap and the Bool unboxing must still
# happen on every store, and a non-Int value must still be checked.

plan 9;

my int32 $a = 2**40 + 5;
is $a, 5, 'int32 wraps a too-large Int';
my uint8 $b = 300;
is $b, 44, 'uint8 wraps 300';
my int16 $c = -40000;
is $c, 25536, 'int16 wraps a too-small Int';
my int8 $d = 127;
$d = $d + 1;
is $d, -128, 'int8 wraps on reassignment';
my uint16 $e = -1;
is $e, 65535, 'uint16 wraps -1';
my int32 $f = True;
is $f, 1, 'a Bool stores as its Int value';

sub loop-sum(--> int32) {
    my int32 $s = 0;
    for ^10 { $s = $s + 2**30 }
    $s
}
is loop-sum(), -2147483648, 'a routine int32 accumulator wraps on every store';

throws-like { my int32 $g = 'x' }, Exception, 'a Str is not an int32';
my int64 $i = 9;
is $i, 9, 'an int64 store is unchanged';
