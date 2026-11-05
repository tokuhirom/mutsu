use Test;
use NativeCall;

plan 5;

# A `CArray[CArray[uint8]]` slot C rewrites reads back as an unmanaged
# `CArray[uint8]` over the address C wrote (OpenSSL's `unsigned char **out`,
# #14894). It used to answer the slot's element type object.
sub strtol(Pointer, CArray[CArray[uint8]], int32 --> long) is native {*}

my $text = CArray[Str].new;
$text[0] = '12abc';
my $start = nativecast(CArray[Pointer], nativecast(Pointer, $text))[0];

my $end = CArray[CArray[uint8]].new;
$end[0] = CArray[uint8];
is strtol($start, $end, 10), 12, 'C parsed the owned string';
my $after = $end[0];
ok $after.defined, 'the slot C wrote reads back as a defined object';
is $after.^name, 'NativeCall::Types::CArray[uint8]', 'of the declared element type';
is $after[0], 'a'.ord, 'whose first byte is where C stopped parsing';
is Buf.new($after[0..2]).decode('utf-8'), 'abc', 'and whose bytes are the rest of the string';
