use Test;

# A sigilless parameter bound to a value (`\obj`) is readonly, but a callee's
# sigiled parameter of the same bare name (`$obj is rw`) is a different
# variable and stays writable (#13207, PDF's `sub (\obj) is rw` accessor
# around `coerce-to($obj is rw, ...)`).

plan 3;

class C { method m($obj is rw) { $obj = "<$obj>" } }
class A { method acc() { my $z = 'p'; C.m($z); $z } }
sub f(\obj) { obj.acc() }
is f(A.new), '<p>', 'a method callee writes its rw param';

sub g($obj is rw) { $obj = 1 }
sub h(\obj) { my $q = 0; g($q); $q }
is h(5), 1, 'a sub callee writes its rw param';

sub k(\obj) { obj = 2 }
throws-like { k(5) }, X::Assignment::RO, 'the sigilless parameter itself stays readonly';
