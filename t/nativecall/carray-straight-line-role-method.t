use v6;
use Test;
use NativeCall;
use nqp;

# A role method whose body is a straight line of local reads and `nqp::` ops
# (CArray's ASSIGN-POS / AT-POS) runs without a method frame (#16215). The
# observable behaviour must not change.

plan 41;

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

# Two compositions with a same-named straight-line method keep their own
# bodies when the calls interleave (the resolved-call table is keyed by the
# composition, not by the method name).
role PlusOne { method bump($a) { nqp::add_i($a, 1) } }
role PlusTwo { method bump($a) { nqp::add_i($a, 2) } }
my $one = 10 but PlusOne;
my $two = 10 but PlusTwo;
is $one.bump(5), 6, 'first composition';
is $two.bump(5), 7, 'second composition';
is $one.bump(5), 6, 'first composition again';
is $two.bump(5), 7, 'second composition again';

# `.Array` / `.list` read every element through a native reference; the
# reference answers the live element, also after the array was written.
my $live = CArray[int32].new;
$live[$_] = $_ * 10 for ^4;
is-deeply $live.list.Array, [0, 10, 20, 30], 'list answers the stored elements';
$live[2] = 99;
is-deeply $live.Array, [0, 10, 99, 30], 'a later store shows in the next .Array';
my $ref := $live[1];
$live[1] = 7;
is $ref, 7, 'a bound element reference reads the live element';
$ref = 8;
is $live[1], 8, 'and writes through to the array';
is-deeply CArray[num64].new.Array, [], 'an empty array lists as empty';

# Reading `$c[$i]` ranks the imported postcircumfix candidates once per
# composition and argument types (#16373); the winner must not leak between
# compositions of one class, nor between element types.
my $r1 = CArray[num64].new; $r1[0] = 1.5e0; $r1[1] = 2.5e0;
my $r2 = CArray[int32].new; $r2[0] = 7;     $r2[1] = 8;
is $r1[0], 1.5e0, 'first read of a num64 CArray';
is $r2[0], 7, 'first read of an int32 CArray';
is $r1[1], 2.5e0, 'second read of the num64 CArray';
is $r2[1], 8, 'second read of the int32 CArray';
is-deeply $r1[0, 1].list, (1.5e0, 2.5e0), 'a list index still takes the Iterable candidate';
is $r2[*].elems, 2, 'a Whatever index still takes its own candidate';

class Plain { }
role Mark-A { }
role Mark-B { }
multi sub which-mark(Mark-A $) { 'A' }
multi sub which-mark(Mark-B $) { 'B' }
my $ma = Plain.new but Mark-A;
my $mb = Plain.new but Mark-B;
is-deeply (which-mark($ma), which-mark($mb), which-mark($ma), which-mark($mb)),
  ('A', 'B', 'A', 'B'),
  'two role compositions of one class keep their own winners when calls interleave';
is-deeply (which-mark($ma), which-mark($mb)), ('A', 'B'), 'and on a second round';

# A cached straight-line call survives a registry write between calls (a new
# class declared at run time) and keeps answering with its own body.
my $w = 3 but PlusOne;
is $w.bump(1), 2, 'before a registry write';
EVAL 'class Late-Declared { method x { 1 } }';
is $w.bump(1), 2, 'after a class was declared at run time';
is $w.bump(5), 6, 'with other arguments';
my $cw = CArray[num64].new; $cw[0] = 1.5e0;
EVAL 'role Late-Role { method y { 2 } }';
is $cw[0], 1.5e0, 'a CArray read after a role was declared at run time';

# A winner picked by the keyed stage program binds without re-checking its
# nominal parameter types (#16373); every index shape still takes its own
# candidate and answers as raku does.
my $ix = CArray[num64].new; $ix[$_] = $_ * 1.5e0 for ^5;
is $ix[2], 3e0, 'an Int index';
is $ix["3"], 4.5e0, 'a Str index goes through the Any candidate';
is $ix[1.0], 1.5e0, 'a Rat index';
is-deeply $ix[1, 2].list, (1.5e0, 3e0), 'a List index';
is-deeply $ix[1..3].list, (1.5e0, 3e0, 4.5e0), 'a Range index';
my int $nat = 3;
is $ix[$nat], 4.5e0, 'a native int index';
