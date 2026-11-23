use Test;

# A call frame saves its caller's per-activation registers (upvalues, capture
# sets, pending caller-variable writebacks, `is rw` rebinds) only when one of
# them is non-empty, and otherwise just clears what the callee left behind
# (#18267 stage 3). Each case below runs a call from a caller that holds one
# of those registers, or a callee that fills one, and checks the caller's
# view after the return, in both orders and nested.
plan 8;

class C {
    has $.n = 0;
    method bump() { $!n++; self }
}

# A closure that captures an outer lexical (upvalues) calls a method, then
# reads its capture again.
my $k = 10;
my &add = -> $x { C.new.bump.n + $x + $k };
is (^3).map({ add($_) }).join(','), '11,12,13', 'a capturing closure calls a method between reads';

# Nested capturing closures, each calling a sub.
sub plain($v) { $v * 2 }
my $base = 1;
my &outer = -> $a {
    my &inner = -> $b { plain($b) + $base };
    inner($a) + $base
};
is (^3).map({ outer($_) }).join(','), '2,4,6', 'nested capturing closures around a call';

# An `is rw` parameter written by the callee, called inside a loop whose body
# also captures.
sub incr($x is rw) { $x++ }
my $count = 0;
for ^4 { incr($count) }
is $count, 4, 'an is rw writeback survives calls in a loop';

# A callee that writes a caller variable through a block (pending caller-var
# writeback), followed by another call from the same caller.
my $desc = 'start';
sub run-block(&b) { b() }
run-block({ $desc = 'changed' });
sub show($d is copy) { $d }
is show($desc), 'changed', 'a block write is seen by the next call';

# Recursion through a capturing closure keeps each level's capture.
my $depth = 0;
my &rec;
&rec = -> $n { $n == 0 ?? $depth !! do { $depth++; rec($n - 1) + 0 } };
is rec(5), 5, 'recursion through a capturing closure';

# A method whose body creates and calls a capturing closure.
class D {
    has $.base = 100;
    method run($x) { my &f = -> $y { $y + $!base + $x }; f(1) + f(2) }
}
is D.new.run(5), 213, 'a capturing closure inside a method';

# A thrown exception unwinds through frames with and without saved registers.
my $seen = 0;
my &thrower = -> $x { $seen += $k; die "boom" if $x == 2; $x };
my @got = (^4).map({ (try thrower($_)) // 'caught' });
is @got.join(','), '0,1,caught,3', 'an exception unwinds a capturing frame';
is $seen, 40, '...and every call saw the capture';
