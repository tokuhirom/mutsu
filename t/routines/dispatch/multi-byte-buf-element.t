use Test;

# A `Buf` element read as a call argument is a native container for a routine
# with a native parameter, so `multi m(byte $a)` wins for `$buf[0]`; a `Blob`
# element stays `Int`, and so does an Array element (#15856, ADR-15309; from
# Net::BGP's `nuint16($!raw[2], $!raw[3])`). Every row is what `raku` answers.

plan 8;

multi m(byte $a) { 'byte' }
multi m(Int $a)  { 'Int' }

my $buf = buf8.new(1, 2, 3);
is m($buf[0]), 'byte', 'buf8 element';
my buf8 $typed = buf8.new(1, 2);
is m($typed[1]), 'byte', 'a buf8-typed variable element';
my blob8 $blob = blob8.new(1, 2);
is m($blob[0]), 'Int', 'blob8 element is boxed';
my buf16 $wide = buf16.new(1);
is m($wide[0]), 'byte', 'buf16 element is the same native family';
my @a = 1, 2;
is m(@a[0]), 'Int', 'Array element';

# The dispatch cache keys the container's native type: one call site, both orders.
my @order;
for $buf, @a, @a, $buf -> $src { @order.push: m($src[0]) }
is @order.join(','), 'byte,Int,Int,byte', 'one call site, both element kinds, both orders';

# Net::BGP's shape.
multi n16(@a where @a.elems == 2 --> Int) { n16(@a[0], @a[1]) }
multi n16(byte $x, byte $y --> Int)       { $x × 2⁸ + $y }
my $raw = buf8.new(0, 1, 7);
is n16($raw[0], $raw[1]), 1, 'n16 over two buf8 elements';
is n16($raw[1], $raw[0]), 256, 'and swapped';
