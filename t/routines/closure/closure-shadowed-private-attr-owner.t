use Test;

# #17936: when a class and its parent both declare `$!x`, a closure made in
# the parent's method reads the parent's copy -- the class it is written in --
# not the most-derived one, whether it runs after the method returned, inside
# the method, or from a method of another class.

plan 7;

class A {
    has $!x = 1;
    method m { $!x }
    method mk { -> { $!x } }
    method in-method { my $f = -> { $!x }; $f() }
    method set($v) { $!x = $v }
    method writer { -> $v { $!x = $v } }
}
class B is A {
    has $!x = 2;
    method bx { $!x }
    method run-closure(&c) { c() }
}

my $b = B.new;
is $b.m, 1, "the parent's method reads the parent's copy";
is $b.mk()(), 1, "a closure the parent's method made reads the parent's copy";
is $b.in-method, 1, 'also when called inside that method';
is $b.run-closure($b.mk), 1, "also when a child's method calls it";
$b.set(5);
is $b.mk()(), 5, "it sees a write to the parent's copy";
$b.writer()(9);
is $b.m, 9, "a write through the closure reaches the parent's copy";
is $b.bx, 2, "and leaves the child's copy alone";
