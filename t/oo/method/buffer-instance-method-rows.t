use Test;

# The `Blob`/`Buf` rows of ADR-11276 §9.107: a user class composing a buffer
# role over a VMArray body reaches the same rows as the built-in buffers, and
# `subbuf`, `subbuf-rw`, `read-*`, `read-ubits`/`read-bits`, `AT-POS` and
# `EXISTS-POS` bind their arguments as Rakudo's candidates do.

plan 53;

sub err(&code) {
    my $r = try code();
    $! ?? $!.^name ~ ': ' ~ $!.message.lines[0] !! $r.raku
}

class B does Blob[uint8] is repr('VMArray') { }
class C does Buf[uint8] is repr('VMArray') { }
class O does Blob[uint8] is repr('VMArray') {
    multi method subbuf(Str:D $x) { "own subbuf $x" }
}

my $b = B.new(1, 2, 3, 4, 250);
my $c = C.new(1, 2, 3, 4, 250);

# A user class over a buffer reaches the rows and keeps its class.
is $b.subbuf(1, 2).raku, 'B.new(2,3)', 'subbuf keeps the user class';
is $b.subbuf(*-2).raku, 'B.new(4,250)', 'subbuf with a WhateverCode';
is $b.read-uint16(0, BigEndian), 258, 'read-uint16 on a user buffer';
is $b.read-ubits(4, 8), 16, 'read-ubits on a user buffer';
is $b.join('-'), '1-2-3-4-250', 'join lists the elements';
like err({ $b.Str }), /^'X::Buf::AsStr: Stringification of a B is not done'/,
    'Str refuses, as for a built-in buffer';
is $b.of.^name, 'uint8', 'of is the element type';
is $c.Blob.raku, 'Blob.new(1,2,3,4,250)', 'Blob of a Buf-composing class';
is $b.EXISTS-POS(1), True, 'EXISTS-POS inside';
is err({ $b.AT-POS(5) }), 'X::OutOfRange: Index out of range. Is: 5, should be in 0..4',
    'AT-POS past the end of a Blob-composing class';
is $c.AT-POS(9), 0, 'AT-POS past the end of a Buf-composing class';
is O.new(1, 2).subbuf("x"), 'own subbuf x', 'a method of the class answers its own candidate';
is O.new(1, 2).subbuf(1).raku, 'O.new(2)', 'and the row the others';

# subbuf binds like Rakudo's candidates.
my $buf = Buf.new(1, 2, 3, 4, 250);
is $buf.subbuf(1^..^4).raku, 'Buf.new(3,4)', 'a range excluding both ends';
is $buf.subbuf(4..2).raku, 'Buf.new()', 'an empty range';
is $buf.subbuf(2..9).raku, 'Buf.new(3,4,250)', 'a range past the end is clamped';
is $buf.subbuf(1, Inf).raku, 'Buf.new(2,3,4,250)', 'an infinite length';
is $buf.subbuf(1, 2.9).raku, 'Buf.new(2,3)', 'a Rat length is truncated';
is $buf.subbuf(1, *).raku, 'Buf.new(2,3,4,250)', 'a Whatever length';
is $buf.subbuf(*-2, *-1).raku, 'Buf.new(4,250)', 'a WhateverCode end';
is err({ $buf.subbuf(10) }), 'X::OutOfRange: "From argument to subbuf out of range. Is: 10, should be in 0..5',
    'a start past the end';
is err({ $buf.subbuf(-1, 1) }), 'X::OutOfRange: "From argument to subbuf out of range. Is: -1, should be in 0..5',
    'a negative start';
is err({ $buf.subbuf(-1..2) }), 'X::OutOfRange: "From argument to subbuf out of range. Is: -1, should be in 0..5',
    'a range with a negative start';
is err({ $buf.subbuf(1, -1) }), 'X::OutOfRange: Len element to subbuf out of range. Is: -1, should be in 0..5',
    'a negative length';
is err({ $buf.subbuf(1..*) }), 'X::AdHoc: Must specify a Range with integer bounds to subbuf',
    'an infinite range';
like err({ $buf.subbuf("2") }), /^'X::Multi::NoMatch: Cannot resolve caller subbuf(Buf:D: Str:D)'/,
    'a Str start has no candidate';
like err({ $buf.subbuf(1, "2") }), /^'X::Multi::NoMatch'/, 'a Str length has no candidate';

# subbuf-rw is Buf's: its rvalue is the window, and a Blob has none.
is $buf.subbuf-rw(1).raku, 'Buf.new(2,3,4,250)', 'subbuf-rw with a start';
is $buf.subbuf-rw().raku, 'Buf.new(1,2,3,4,250)', 'subbuf-rw with no argument';
is $buf.subbuf-rw(1, *-1).raku, 'Buf.new(2,3,4,250)', 'subbuf-rw with a WhateverCode end';
like err({ Blob.new(1, 2).subbuf-rw(0, 1) }), /^'X::Method::NotFound'/, 'a Blob has no subbuf-rw';
like err({ utf8.new(65).subbuf-rw(0, 1) }), /^'X::Method::NotFound'/, 'a utf8 has no subbuf-rw';
{
    my $w = Buf.new(1, 2, 3);
    $w.subbuf-rw(1, 1) = Buf.new(7, 7);
    is $w.raku, 'Buf.new(1,7,7,3)', 'subbuf-rw as an lvalue';
}

# The read-* rows bind `int $offset` and `Endian $endian`.
is err({ $buf.read-uint8(-1) }), 'X::AdHoc: MVMArray: read_buf out of bounds offset -1 start 0 elems 5 count 1',
    'a negative offset is reported as given';
is err({ $buf.read-uint8("1") }), 'X::AdHoc: This type cannot unbox to a native integer: P6opaque, Str',
    'a Str offset';
is err({ $buf.read-uint8(1e0) }), 'X::AdHoc: This type cannot unbox to a native integer: P6opaque, Num',
    'a Num offset';
is err({ $buf.read-uint8(Int) }), 'X::AdHoc: Cannot unbox a type object (Int) to int.', 'a type object offset';
is $buf.read-uint8(True), 2, 'a Bool offset is an Int';
is err({ $buf.read-uint16(0, 1) }), 'X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \'$endian\'; expected Endian but got Int (1)',
    'an Int is not an Endian';

# read-ubits / read-bits.
is $buf.read-bits(32, 8), -6, 'read-bits sign-extends';
is $buf.read-ubits(40, 0), 0, 'zero bits at the end';
is $buf.read-bits(0, -2), 0, 'a negative bit count reads nothing';
# Rakudo also names the receiver's variable ("in buffer '$buf'"), which a
# method does not know.
like err({ $buf.read-ubits(0, 99) }), /^'X::AdHoc: Can only read 1..40 bits from position 0 in buffer' .* ', you tried: 99'$/,
    'too many bits';
like err({ $buf.read-ubits(-1, 3) }), /^'X::AdHoc: Can only read from position 0..39 in buffer' .* ', you tried: -1'$/,
    'a negative position';
is err({ $buf.read-ubits(1.5, 2) }), 'X::AdHoc: This type cannot unbox to a native integer: P6opaque, Rat',
    'a Rat position';

# AT-POS / EXISTS-POS: a Buf reads 0 past its end, a Blob raises.
is $buf.AT-POS(10), 0, 'Buf.AT-POS past the end';
is err({ Blob.new(1, 2, 3).AT-POS(10) }), 'X::OutOfRange: Index out of range. Is: 10, should be in 0..2',
    'Blob.AT-POS past the end';
is err({ $buf.AT-POS(-1) }), 'X::OutOfRange: Index out of range. Is: -1, should be in 0..4',
    'a negative position';
is $buf.EXISTS-POS(4), True, 'EXISTS-POS of the last element';
is $buf.EXISTS-POS(5), False, 'EXISTS-POS past the end';
is $buf.EXISTS-POS("0"), True, 'a Str index is `Any`\'s: .Int == 0';
is Buf.EXISTS-POS(0), False, 'a type object has no elements';

# The coercions keep the element type.
is buf16.new(1, 2).Blob.raku, 'Blob[uint16].new(1,2)', 'buf16.Blob';
