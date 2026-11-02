use Test;
use NativeCall;

plan 5;

# #15985: an EMPTY `CArray[CArray[uint8]].new` handed to C as an out-parameter
# (`char **endptr`) used to be marshalled as NULL, and the callee crashed
# writing through it. It now gets one slot C can write through, and the
# element still reads back as the declared `CArray[uint8]`.
sub strtol(Pointer, CArray[CArray[uint8]], int32 --> long) is native {*}

my $text = CArray[Str].new;
$text[0] = '12abc';
my $start = nativecast(CArray[Pointer], nativecast(Pointer, $text))[0];

my $end = CArray[CArray[uint8]].new;       # no element assigned first
is strtol($start, $end, 10), 12, 'C parsed the owned string, no crash';
is $end[0].^name, 'NativeCall::Types::CArray[uint8]', 'the slot reads back as the declared element type';

# Another call with a fresh empty array does not see the earlier slot.
my $second = CArray[CArray[uint8]].new;
is strtol($start, $second, 10), 12, 'a second empty array works too';

# The assigned-first spelling keeps working.
my $assigned = CArray[CArray[uint8]].new;
$assigned[0] = CArray[uint8];
is strtol($start, $assigned, 10), 12, 'with an element assigned first';
is $assigned[0][0], 'a'.ord, 'whose slot C wrote reads back';
