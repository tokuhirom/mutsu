use Test;

# #13312: `$blob[$i]` reads what `$blob.AT-POS($i)` reads (the `Blob`/`Buf`
# rows, `blob_pos::subscript_at`): a `Blob` past its end is an
# `X::OutOfRange` failure, a resizable `Buf` reads `0` there, and a negative
# position is out of range for both.

plan 12;

class UB does Blob[uint8] is repr('VMArray') { }

throws-like { Blob.new(1, 2)[5] }, X::OutOfRange,
    message => 'Index out of range. Is: 5, should be in 0..1', 'Blob past its end';
throws-like { utf8.new(65)[3] }, X::OutOfRange,
    message => 'Index out of range. Is: 3, should be in 0..0', 'utf8 past its end';
throws-like { blob16.new(1, 2)[2] }, X::OutOfRange, 'blob16 past its end';
throws-like { UB.new(1, 2)[5] }, X::OutOfRange, 'a user Blob class past its end';
is Buf.new(1, 2)[5], 0, 'a Buf reads 0 past its end';
is buf16.new(1)[3], 0, 'a buf16 reads 0 past its end';
my $neg = -1;
throws-like { Buf.new(1, 2)[$neg] }, X::OutOfRange, 'a negative position on a Buf';
throws-like { Blob.new(1, 2)[$neg] }, X::OutOfRange, 'a negative position on a Blob';

# The failure is returned, not thrown: it explodes when used.
my $f = Blob.new(1)[5];
isa-ok $f, Failure, 'the out-of-range read is a Failure';
$f.so;
isa-ok Blob.new(1).AT-POS(5), Failure, 'and so is the method form';

is Blob.new(1, 2)[1], 2, 'an element in range';
is UB.new(7, 8)[0], 7, 'a user Blob class element in range';
