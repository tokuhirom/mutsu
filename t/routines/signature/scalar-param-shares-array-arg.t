use Test;

# An `@`/`%` variable passed to a plain `$` parameter binds the caller's own
# container: mutations through the parameter are the caller's. The light call
# paths serve this shape directly now (#15786); these pin the semantics.

plan 16;

my @a = 1, 2, 3;
sub f1($n) { $n.push(4) }
f1(@a); is @a.join(","), "1,2,3,4", '.push through a $ parameter';
sub f2($n) { $n[0] = 10 }
f2(@a); is @a.join(","), "10,2,3,4", 'element store';
sub f3($n) { my @b := @$n; @b.push(5) }
f3(@a); is @a.join(","), "10,2,3,4,5", 'rebound through @$n';
sub f4($n) { $n.pop }
f4(@a); is @a.join(","), "10,2,3,4", '.pop';
sub f5($n) { $n[1]++ }
f5(@a); is @a.join(","), "10,3,3,4", 'element ++';
sub f6($n) { for @$n { $_ *= 2 } }
f6(@a); is @a.join(","), "20,6,6,8", 'elements written through a for loop';
sub f7($n) { $n.splice(0, 1) }
f7(@a); is @a.join(","), "6,6,8", '.splice';
sub f8($n) { $n = 5 }
throws-like { f8(@a) }, Exception, 'the parameter itself stays read-only';
sub c1($n) { my $c = { $n.push(9) }; $c() }
c1(@a); is @a.join(","), "6,6,8,9", 'through a closure';
sub g($m) { $m.push(1) }
sub c2($n) { g($n) }
c2(@a); is @a.join(","), "6,6,8,9,1", 'through a nested call';
sub c9($n, $m) { $n.push(1); $m.elems }
is c9(@a, @a), 6, 'two parameters see one container';
sub c10($n) { my @c = @$n; @c.push(100); @c.elems }
is c10(@a), 7, 'a copy into a fresh array';
is @a.elems, 6, 'leaves the caller alone';
sub c14($n is copy) { $n.push(42); $n.elems }
is c14(@a), 7, 'is copy';

my %h = x => 1;
sub g1($n) { $n<y> = 2 }
g1(%h); is %h.keys.sort.join(","), "x,y", 'a hash: key store';
sub g2($n) { $n<x>:delete }
g2(%h); is %h.keys.sort.join(","), "y", ':delete';
