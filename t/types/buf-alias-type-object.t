use Test;

plan 15;

# buf16 & co are constants bound to the parameterized types in Rakudo, so the
# type object itself is `Buf[uint16]` (#13314).
is buf16.raku, 'Buf[uint16]', 'buf16.raku';
is buf16.gist, '(Buf[uint16])', 'buf16.gist';
is blob8.^name, 'Blob[uint8]', 'blob8.^name';
is buf8.WHAT.raku, 'Buf[uint8]', 'buf8.WHAT';
is blob64.raku, 'Blob[uint64]', 'blob64.raku';
is buf16.new(1).raku, 'Buf[uint16].new(1)', 'instance rendering is unchanged';
is buf32.new(1).WHAT.raku, 'Buf[uint32]', 'instance type';
ok buf16 === Buf[uint16], 'identical to the parameterized type';
ok Buf[uint16] ~~ buf16, 'smartmatch with the parameterized type';
ok buf8 ~~ Buf, 'is a Buf';
ok blob8 ~~ Blob, 'is a Blob';
ok buf64.new(1) ~~ Buf, 'instance is a Buf';

my buf8 $b = buf8.new(1, 2);
is $b.elems, 2, 'declared with the alias type';
sub f(buf16 $x) { $x.elems }
is f(buf16.new(1, 2, 3)), 3, 'alias in a signature';
nok buf16.defined, 'type object is undefined';
