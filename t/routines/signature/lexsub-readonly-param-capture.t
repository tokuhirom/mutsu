use Test;

# A sub declared inside a routine reads its declaring routine's readonly
# signature parameters (no `is copy`/`rw`/`raw`). Such a binding is fixed for
# the invocation, so mutsu aliases the value itself rather than boxing the
# parameter into a shared cell, and a closure the nested sub creates captures
# that value. Every answer here is rakudo's; the cell made threads that ran
# the nested sub's closures contend on one lock (#17337).

plan 9;

sub f1($x) { sub g { $x }; my $c = { g() }; $c() }
is f1(3), 3, 'a nested sub reads the readonly parameter';

sub f2(&by, $d) { sub p { start { by($d, 1) } }; await p() }
is f2(&[cmp], 2), More, 'a closure cued by the nested sub sees &by and $d';

class C { method m(int $d) { sub p { { my $c = { $d }; $c() } }; p() } }
is C.m(7), 7, 'a native readonly method parameter reaches the nested sub';

sub f3($x) { sub g { my $x = 9; my $c = { $x }; $x = 10; $c() }; g() }
is f3(1), 10, 'the nested sub\'s own same-named my still tracks its writes';

sub f4($n) { sub g { $n }; $n > 0 ?? (f4($n - 1) ~ g()) !! g() }
is f4(3), '0123', 'each activation\'s nested sub sees its own parameter';

my &kept;
sub f5($x) { sub g { $x }; &kept = &g }
f5(5); f5(6);
is kept(), 6, 'an escaped nested sub sees the latest activation';

sub f6($x) { sub g { -> { $x } }; g() }
my $a = f6(1);
my $b = f6(2);
is $a() ~ $b(), '12', 'closures from two activations keep their own values';

sub f7($x) { sub g { $x = 5 }; g() }
dies-ok { f7(1) }, 'assigning the readonly parameter in the nested sub dies';

sub f8($x is copy) { sub g { $x = 5 }; g(); $x }
is f8(1), 5, 'an is-copy parameter is still a shared container';
