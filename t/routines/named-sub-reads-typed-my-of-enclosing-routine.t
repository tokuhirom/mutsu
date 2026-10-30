use Test;

# LLM::RetrievalAugmentedGeneration::VectorDatabase.join: `my Bool:D $commonKeys = ...;
# sub joined-key(...) { return $key if !$commonKeys; ... }`. A named sub declared in a
# routine read a constrained scalar of that routine (`Bool`, `Any`, `Array`, ...) as `Nil`
# on every call: the hoisted pass had seeded the sub's alias with a `Nil` cell, and for a
# variable that cannot be boxed the declaration never replaced it.

plan 18;

sub a1() { my Bool $c = True; sub f() { $c }; f() }
sub a2() { my Int  $c = 5;    sub f() { $c }; f() }
is a1(), True, 'a Bool read by a nested named sub';
is a2(), 5, 'an Int is unchanged';

sub b1() { my Bool:D $c = True; sub f() { $c }; f() }
sub b2() { my Any  $c = 'any'; sub f() { $c }; f() }
sub b3() { my Cool $c = 'cool'; sub f() { $c }; f() }
sub b4() { my Array $c = [1, 2]; sub f() { $c }; f() }
is b1(), True, 'Bool:D';
is b2(), 'any', 'Any';
is b3(), 'cool', 'Cool';
is-deeply b4(), [1, 2], 'Array';

sub c1() { my Bool $c = True; sub f() { $c }; $c = False; f() }
sub c2() { my Bool $c = True; sub f() { $c }; my $before = f(); $c = False; ($before, f()) }
sub c3() { my Bool $c = True; sub f() { $c = False }; f(); $c }
sub c4() { my Any $c = 1; sub f() { $c }; $c = 2; f() }
is c1(), False, 'an assignment after the declaration is seen by the sub';
is-deeply c2(), (True, False), 'and the earlier call saw the earlier value';
is c3(), False, 'a write from the sub reaches the variable';
is c4(), 2, 'Any, assigned after the declaration';

sub d($n) { my Bool $c = $n > 1; sub f() { $c }; f() }
is d(1), False, 'a computed value, false';
is d(2), True, 'a computed value, true';

sub e() { my Bool $c = True; sub f() { $c }; sub g() { f() }; g() }
is e(), True, 'a sub reading it through another nested sub';

# The shape of VectorDatabase.join: the flag differs between the calls of one method.
class V {
    has %.vectors;
    method join($obj) {
        my Bool:D $common = (%!vectors.keys (&) $obj.vectors.keys).elems > 0;
        my $offset = %!vectors.elems;
        sub joined-key(Str:D $key, UInt:D $off) {
            return $key if !$common;
            return ($key.Int + $off).Str;
        }
        for $obj.vectors.keys.sort -> $key { %!vectors{joined-key($key, $offset)} = 1 }
        self
    }
}
my $a = V.new(vectors => { '0' => 1, '1' => 1 });
my $res = V.new;
$res.join($a);
is $res.vectors.keys.sort.join(','), '0,1', 'the first join into an empty database';
$res.join($a);
is $res.vectors.keys.sort.join(','), '0,1,2,3', 'the second join renumbers the keys';
$res.join($a);
is $res.vectors.keys.sort.join(','), '0,1,2,3,4,5', 'and so does the third';
is $res.vectors.elems, 6, 'six vectors in total';

# An untyped and an Int/Str variable were always fine.
sub f1() { my $c = True; sub f() { $c }; f() }
sub f2() { my Str $c = 's'; sub f() { $c }; f() }
is f1() ~ f2(), 'Trues', 'untyped and Str stay unchanged';
