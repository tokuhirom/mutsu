use Test;

plan 9;

# #10959: the fast method path keeps up to two bound parameters inline and spills
# the rest; every parameter count still binds in order and stays readonly.
class C {
    has $.v = 10;
    method m0() { 0 }
    method m1($a) { $a }
    method m2($a, $b) { "$a$b" }
    method m3($a, $b, $c) { "$a$b$c" }
    method m5($a, $b, $c, $d, $e) { "$a$b$c$d$e" }
    method ro($a) { $a = 1 }
    method rw($a is rw) { $a = 5 }
    method named($a, :$b = 'n') { "$a$b" }
}
my $o = C.new;
is $o.m0, 0, 'no parameter';
is $o.m1(1), 1, 'one parameter';
is $o.m2(1, 2), '12', 'two parameters (both inline)';
is $o.m3(1, 2, 3), '123', 'three parameters (one spills)';
is $o.m5(1, 2, 3, 4, 5), '12345', 'five parameters keep their order';
dies-ok { $o.ro(5) }, 'a plain parameter stays readonly';
my $x = 0;
$o.rw($x);
is $x, 5, 'an is rw parameter writes through';
is $o.named(1, :b<z>), '1z', 'a named parameter next to a positional';
my $s = 0;
$s += $o.m1($_) for ^5;
is $s, 10, 'repeated calls with an argument';
