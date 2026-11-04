use v6;
use Test;
use NativeCall;
use nqp;

# A role method whose body is a straight line of local reads and `nqp::` ops
# (CArray's ASSIGN-POS / AT-POS) runs without a method frame (#16215). The
# observable behaviour must not change.

plan 14;

my $n = CArray[num64].new;
$n[$_] = $_ * 1.5e0 for ^6;
is $n.elems, 6, 'num64 CArray holds six elements';
is-deeply $n.Array, [0e0, 1.5e0, 3e0, 4.5e0, 6e0, 7.5e0], '.Array reads every stored element';
is $n[3], 4.5e0, 'an element read answers the stored value';

$n[1] = 9e0;
is $n[1], 9e0, 'a store overwrites in place';
is $n.elems, 6, 'an overwrite does not grow the array';

my $i = CArray[int32].new;
$i[0] = 300;
$i[1] = -7;
is-deeply $i.Array, [300, -7], 'int32 CArray round-trips';

my $u = CArray[uint8].new;
$u[0] = 255;
is $u[0], 255, 'uint8 keeps its range';

# The same shape on a user role: a body of local reads and one nqp op.
role Doubler { method twice($a) { nqp::add_i($a, $a) } }
my $d = 5 but Doubler;
is $d.twice(4), 8, 'a user role method on a mixin runs';
is $d.twice(21), 42, 'a second call with other arguments';
is $d.twice(4), 8, 'and the first arguments again (memoized winner)';

# A role method that does observe the role context keeps the full path.
role Param[::T] { method t { T } }
my $q = 1 but Param[Str];
is $q.t.^name, 'Str', 'a body reading a role type parameter still sees it';

# Calls from inside a method frame leave the caller's locals alone.
sub fill($c, $len) {
    my $sum = 0;
    for ^$len -> $k { $c[$k] = $k * 1e0; $sum += $c[$k] }
    $sum
}
my $f = CArray[num64].new;
is fill($f, 10), 45e0, 'a loop of stores and reads keeps the caller state';
is $f.elems, 10, 'the stores landed';

my $x = CArray[num64].new;
$x[2] = 2e0;
is-deeply $x.Array[0..2].list, (0e0, 0e0, 2e0), 'a gap is zero-filled';
