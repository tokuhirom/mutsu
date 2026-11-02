use Test;

# From the Archive::Tar::PP ecosystem distribution:
# `Buf.new(Buf.allocate($n, 0).Slip, ...)` -- `.Slip` on a Buf slips its
# elements (a Buf is Positional), it is not a one-element Slip of the Buf.

plan 6;

is Buf.new(1, 2, 3).Slip.elems, 3, 'Buf.Slip has one entry per element';
is blob8.new(1, 2).Slip.elems, 2, 'blob8.Slip too';
is Buf.allocate(4, 0).Slip.elems, 4, 'Buf.allocate(4, 0).Slip';
is Buf.allocate(0).Slip.elems, 0, 'an empty Buf slips nothing';

my $val = "14 ".encode('utf8');
$val .= new(Buf.allocate(12 - $val.elems, 0).Slip, $val.values.Slip);
is-deeply $val.list.List, (0 xx 9, 49, 52, 32).flat.List, 'left-padding a Buf with .new(pad.Slip, old.values.Slip)';

my @a = Buf.allocate(2, 7).Slip, 5;
is-deeply @a, [7, 7, 5], 'slips flatten into a list';
