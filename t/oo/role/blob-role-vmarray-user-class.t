use Test;

# A user class that composes a buffer role over a VMArray body
# (`class B does Blob[uint8] is repr('VMArray')`, PDF's `PDF::IO::Blob`) is a
# buffer: its instances take the buffer methods, keep the user's class name
# and methods, and type-check as both the class and the role (#9438).

plan 20;

class B does Blob[uint8] is repr('VMArray') {
    method codes { self.bytes }
}

my $b = B.new(65, 66, 67);
is $b.bytes, 3, '.bytes';
is $b.elems, 3, '.elems';
is $b.decode('latin-1'), 'ABC', '.decode';
ok $b ~~ Blob, 'smartmatches Blob';
ok $b ~~ B, 'smartmatches its own class';
is $b.codes, 3, 'a user method sees the buffer through self';
is $b.^name, 'B', '.^name is the user class';
is $b[1], 66, 'positional read';
is $b.list, (65, 66, 67), '.list';
is $b.subbuf(1).^name, 'B', '.subbuf keeps the class';
is ($b ~ B.new(68)).^name, 'B', '~ keeps the class';
ok $b eqv B.new(65, 66, 67), 'eqv an equal buffer of the same class';
is B.new.elems, 0, 'an empty instance';
is B.^mro.map(*.^name).join(' '), 'B Any Mu', 'the role is not an inheritance parent';

class C does Buf[uint8] is repr('VMArray') { }
my $c = C.new(65, 66);
$c.push(67);
is $c.decode, 'ABC', 'a Buf-composing class is mutable';
is $c.raku, 'C.new(65,66,67)', '.raku names the user class';
is $c.gist, 'C:0x<41 42 43>', '.gist names the user class';

class W does Blob[uint16] is repr('VMArray') { }
is W.new(1, 65535).bytes, 4, 'the element type comes from the composed role';

# The shared predicate also covers the encoding buffers, which the hand-rolled
# truthiness check had missed.
nok ''.encode.Bool, 'an empty utf8 is false';
ok 'a'.encode.Bool, 'a non-empty utf8 is true';
