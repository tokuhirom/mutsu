use Test;
use nqp;

# The non-FFI `nqp::` ops upstream NativeCall uses (ADR-11203, #11206).

plan 18;

# unbox_n: a Num unboxes to its native num; an Int is a different REPR.
is nqp::unbox_n(1.5e0), 1.5e0, 'unbox_n on a Num';
dies-ok { nqp::unbox_n(3) }, 'unbox_n refuses an Int, as MoarVM does';

# unbox_u: an Int read as a native uint64, wrapping modulo 2**64.
is nqp::unbox_u(3), 3, 'unbox_u on a small Int';
is nqp::unbox_u(-1), 18446744073709551615, 'unbox_u wraps -1 to 2**64 - 1';
is nqp::unbox_u(2**64 - 1), 18446744073709551615, 'unbox_u keeps the uint64 maximum';

# atpos_u / bindpos_u on a native uint64 array and on Bufs.
my uint64 @l = 1, 2, 3;
nqp::bindpos_u(@l, 1, 2**64 - 1);
is nqp::atpos_u(@l, 1), 18446744073709551615, 'bindpos_u/atpos_u round-trip the uint64 maximum';
is nqp::atpos_u(Buf[uint8].new(255, 1), 0), 255, 'atpos_u decodes a Buf element at its width';
my $b := Buf[uint16].new(1, 2);
nqp::bindpos_u($b, 0, 65535);
is $b[0], 65535, 'bindpos_u encodes into a Buf[uint16]';
nqp::bindpos_u($b, 1, -1);
is $b[1], 65535, 'bindpos_u of -1 stores all ones at the Buf width';

# atposref_*: an lvalue for one element; writes land in the list.
my int @a = 10, 20, 30;
my $r := nqp::atposref_i(@a, 1);
is $r, 20, 'atposref_i reads the element';
$r = 99;
is @a.join(','), '10,99,30', 'a write through atposref_i lands in the array';
my num @n = 1e0, 2e0;
my $rn := nqp::atposref_n(@n, 0);
$rn = 5e0;
is @n.join(','), '5,2', 'a write through atposref_n lands in the array';
my int @plain = 1, 2, 3;
my $last := nqp::atposref_i(@plain, -1);
$last = 7;
is @plain.join(','), '1,2,7', 'atposref_i counts a negative index from the end';
my uint64 @u = 1, 2;
my $ru := nqp::atposref_u(@u, 1);
$ru = 5;
is @u.join(','), '1,5', 'a write through atposref_u lands in the array';

# setcodename / neverrepossess.
my $s := sub foo { 42 };
lives-ok { nqp::setcodename(nqp::getattr($s, Code, '$!do'), 'bar') },
    'setcodename renames the code ref behind a routine';
is $s.name, 'bar', 'the routine reports the new name';
is $s(), 42, 'the routine still runs';
ok nqp::eqaddr(nqp::neverrepossess($s), $s), 'neverrepossess hands back its operand';
