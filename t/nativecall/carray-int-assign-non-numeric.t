use Test;
use NativeCall;

plan 7;

# Assigning a non-numeric Str into an integer CArray element dies like
# rakudo (X::AdHoc from the failed unbox), instead of storing address 0 (#16420).
for int32, int8, uint16, int64 -> \T {
    my $c = CArray[T].new;
    throws-like { $c[0] = "x" }, X::AdHoc, "CArray[{T.^name}] rejects a non-numeric Str";
}

my $i = CArray[int32].new;
$i[0] = "7";
is $i[0], 7, 'a numeric Str still coerces';
$i[1] = 5;
is $i[1], 5, 'an Int is stored';
my $n = CArray[num64].new;
throws-like { $n[0] = "x" }, X::AdHoc, 'CArray[num64] rejects a non-numeric Str';
