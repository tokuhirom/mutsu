use Test;
use nqp;

# `nqp::atposref_i` / `_u` / `_n` on native storage answer MoarVM's native
# reference containers (`IntPosRef`, `UIntPosRef`, `NumPosRef`), which read
# and write the element's bytes (#11209).

plan 11;

my $b := buf8.new(1, 2, 3);
my $r := nqp::atposref_i($b, 0);
is $r.VAR.^name, 'IntPosRef', 'atposref_i answers an IntPosRef';
is $r, 1, 'it reads the element';
$r = 5;
is $b[0], 5, 'a write through it lands in the buffer';
$b[0] = 7;
is $r, 7, 'it reads the live element, not a snapshot';

my $last := nqp::atposref_i($b, -1);
$last = 9;
is $b[2], 9, 'a negative index counts from the end';

my $w := Buf[uint32].new(0);
my $u := nqp::atposref_u($w, 0);
is $u.VAR.^name, 'UIntPosRef', 'atposref_u answers a UIntPosRef';
$u = -1;
is $w[0], 4294967295, 'a write is encoded at the buffer width';

my $grow := nqp::atposref_i($b, 5);
is $grow, 0, 'an element past the end reads as zero';
$grow = 4;
is $b.elems, 6, 'a write past the end grows the buffer';

# A Proxy subclass is named after the subclass.
class P is Proxy { }
my $p := P.new(FETCH => -> $ { 1 }, STORE => -> $, $ { });
is $p.VAR.^name, 'P', 'a Proxy subclass reports its own name';
is $p, 1, 'and FETCHes as usual';
