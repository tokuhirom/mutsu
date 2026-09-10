use Test;
use NativeCall;

class MVMArrayB is repr('CStruct') {
    has uint64 $.elems;
    has uint64 $.start;
    has uint64 $.ssize;
    has Pointer $.any;
}

plan 10;

my int @a = 10, 20;
is @a.REPR, 'VMArray', 'native array reports VMArray';
ok @a.WHERE > 0, 'native array has a body address';
my $body = nativecast(MVMArrayB, Pointer.new(@a.WHERE));
is $body.elems, 2, 'body reports native array element count';
is $body.start, 0, 'native array body has zero start offset';
my $payload = nativecast(CArray[int64], $body.any);
is $payload[0], 10, 'body payload points at array storage';
$payload[1] = 42;
is @a[1], 42, 'C writes through retained payload are visible in Raku';

# ADR-0030 §1.2: a Raku write to a *different* index must not silently
# discard a pending C write that has not been read back into Raku yet.
my int @b = 10, 20, 30;
my $bbody = nativecast(MVMArrayB, Pointer.new(@b.WHERE));
my $bpayload = nativecast(CArray[int64], $bbody.any);
$bpayload[2] = 99;
@b[0] = 7;
is "@b[0] @b[2]", "7 99",
    'a Raku write to another index does not discard an unread C write';

# ADR-0030 §1.3-3: repeated reads after a C write must keep seeing the
# synced value (exercises the read path's snapshot comparison).
my int @c = 1, 2, 3;
my $cbody = nativecast(MVMArrayB, Pointer.new(@c.WHERE));
my $cpayload = nativecast(CArray[int64], $cbody.any);
$cpayload[1] = 55;
my $sum = 0;
for ^3 -> $i { $sum += @c[$i] }
is $sum, 1 + 55 + 3, 'repeated reads after a native write stay consistent';

my num @n = 1e0, 2e0;
is @n.REPR, 'VMArray', 'native num array reports VMArray';
is @n[1], 2e0, 'native num array remains readable';
