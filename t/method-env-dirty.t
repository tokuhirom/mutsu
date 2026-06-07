use v6;
use Test;

# Regression pin for Slice 6.3: gating the blanket per-method-call env_dirty=true.
# A pure (read-only) compiled method call must do zero env->locals re-sync, but a
# method that mutates a captured-outer variable, an attribute the caller then
# reads, or that routes through the interpreter bridge must still keep the
# caller's locals coherent. See docs/vm-dual-store.md (Slice 6.3).

plan 20;

# --- read-only method, caller interleaves reads of its own locals ---
class P { has $.x; method g() { return $!x + 1 } }
my $p = P.new(x => 5);
my $s = 0;
my $own = 0;
for ^100 {
    $s += $p.g();
    $own += 1;
}
is $s, 600, 'read-only method result correct across loop';
is $own, 100, 'caller local untouched by read-only method calls';

# --- method mutating its own attribute; caller reads attr after ---
class Counter { has $.n is rw; method bump() { $!n++ } }
my $c = Counter.new(n => 0);
$c.bump(); $c.bump(); $c.bump();
is $c.n, 3, 'attribute mutation via method visible to caller';

# --- method mutating a captured-outer scalar ---
my $outer = 0;
class Pusher { method go() { $outer += 10 } }
my $pu = Pusher.new;
$pu.go(); $pu.go();
is $outer, 20, 'captured-outer scalar mutation via method propagates';

# --- method mutating a captured-outer array/hash ---
my @log;
class Logger { method add($x) { @log.push($x) } }
my $lg = Logger.new;
$lg.add('a'); $lg.add('b'); $lg.add('c');
is @log.join(','), 'a,b,c', 'captured-outer array mutation via method propagates';

my %seen;
class Marker { method mark($k) { %seen{$k} = True } }
my $mk = Marker.new;
$mk.mark('x'); $mk.mark('y');
is %seen.keys.sort.join(','), 'x,y', 'captured-outer hash mutation via method propagates';

# --- interleaved: caller local read between a mutating method and its result ---
my $acc = 0;
my $tally = 0;
class Adder { has $.step; method add() { $acc += $!step; return $!step } }
my $ad = Adder.new(step => 3);
for ^5 {
    $tally += $ad.add();
}
is $acc, 15, 'interleaved captured-outer mutation across method-call loop';
is $tally, 15, 'method return value correct across loop';

# --- nested method calls: inner mutates captured outer, outer reads ---
my $deep = 0;
class Nest {
    method inner() { $deep++ }
    method outer() { self.inner(); self.inner() }
}
my $ne = Nest.new;
$ne.outer(); $ne.outer();
is $deep, 4, 'nested method calls propagate captured mutation';

# --- pure method call after a mutation: caller must not read stale local ---
my $state = 100;
class Mutator { method mutate() { $state = 200 } }
class Pure { method calc($n) { $n * 2 } }
my $mu = Mutator.new;
my $pr = Pure.new;
$mu.mutate();
my $r = $pr.calc(5);
is $state, 200, 'outer mutation visible after an interleaved pure method call';
is $r, 10, 'pure method call result correct';

# --- native-mutating method on a captured outer array ---
my @arr = 1, 2, 3;
class Sorter { method sortit() { @arr = @arr.reverse } }
my $so = Sorter.new;
$so.sortit();
is @arr.join(','), '3,2,1', 'method reassigning captured-outer array propagates';

# --- method returning a value used to index a caller local ---
class Idx { method pick() { 2 } }
my @data = 10, 20, 30, 40;
my $id = Idx.new;
my $picked = @data[$id.pick()];
is $picked, 30, 'method result usable as caller-local index';

# --- attribute mutation then immediate read in same expression chain ---
class Bank { has $.bal is rw; method deposit($a) { $!bal += $a; return $!bal } }
my $bk = Bank.new(bal => 0);
my $b1 = $bk.deposit(50);
my $b2 = $bk.deposit(25);
is $b1, 50, 'first deposit returns running balance';
is $b2, 75, 'second deposit returns running balance';
is $bk.bal, 75, 'final balance via accessor matches';

# --- native read method (.elems) on a variable receiver, interleaved with a
#     caller-local read across a loop (the CallMethodMut native-read pure case) ---
my @nums = 10, 20, 30;
my $sum-elems = 0;
my $iters = 0;
for ^50 {
    $sum-elems += @nums.elems;
    $iters += 1;
}
is $sum-elems, 150, 'native .elems read on variable correct across loop';
is $iters, 50, 'caller local untouched by native-read method calls';

# --- mutating-native (.push) on a variable interleaved with reading another
#     caller local: the mutation must be visible and the other local intact ---
my @acc-arr;
my $count = 0;
for 1..6 -> $i {
    @acc-arr.push($i);
    $count += 1;
}
is @acc-arr.join(','), '1,2,3,4,5,6', 'native .push mutation visible across loop';
is $count, 6, 'caller local intact alongside native push mutation';
