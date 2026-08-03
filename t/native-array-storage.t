use Test;
use NativeCall;

class MVMArrayB is repr('CStruct') {
    has uint64 $.elems;
    has uint64 $.start;
    has uint64 $.ssize;
    has Pointer $.any;
}

plan 8;

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

my num @n = 1e0, 2e0;
is @n.REPR, 'VMArray', 'native num array reports VMArray';
is @n[1], 2e0, 'native num array remains readable';
