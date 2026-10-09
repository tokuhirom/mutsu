use Test;
# A user override on an `is SetHash`/`is BagHash`/`is MixHash` subclass that
# defers (callsame) must reach the MUTATING native rows, not a pure copy
# (set, unset, add, remove, grab).

plan 6;

class S is SetHash { method set(|c) { callsame } }
my $s = S.new;
$s.set('q');
ok $s<q>, 'set override defers and mutates the SetHash';

class U is SetHash { method unset(|c) { callsame } }
my $u = U.new('a', 'b');
$u.unset('a');
nok $u<a>, 'unset override defers and removes the element';
ok $u<b>, 'the other element stays';

class B is BagHash { method add(|c) { callsame } }
my $b = B.new;
$b.add('x');
$b.add('x');
is $b<x>, 2, 'add override defers and increments the weight';

class R is BagHash { method remove(|c) { callsame } }
my $r = R.new('y', 'y');
$r.remove('y');
is $r<y>, 1, 'remove override defers and decrements the weight';

class G is BagHash { method grab(|c) { callsame } }
my $g = G.new('z');
$g.grab(1);
is $g.elems, 0, 'grab override defers and removes the element';
