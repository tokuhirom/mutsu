use Test;
use lib 't/lib';
use IncdecRwCellDeadlock;

# `$pos++` on an `is rw` parameter (three methods deep) used to deadlock on the
# parameter's own non-reentrant cell mutex once a `CArray[num64]` element had
# been stored earlier: the in-place increment held the cell lock while it
# looked the variable's type constraint up through the env, which reads the
# same cell again (MongoDB::Fast `BSON.decode`).

plan 2;

my $m = IncdecRwCellDeadlock.new;
$m.put(1e0);
is $m.decode(Buf.new(1, 120, 0, 5)), 'k', 'a nested is-rw increment after a CArray[num64] store returns';
is $m.decode(Buf.new(2)), 'k', 'and keeps working on a second call';
