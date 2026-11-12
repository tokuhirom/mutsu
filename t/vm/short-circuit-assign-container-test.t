use Test;

# `//=`, `||=` and `&&=` return the left-hand container when they keep its
# value and it has one, and the bare value when it does not (the
# `VarIsWritable` test the desugar branches on).

plan 8;

my $a = 52;
($a //= 42) += 10;
is $a, 62, 'a kept container is returned and assigned through';

my $b := 42;
throws-like { ($b //= 42) += 10 }, X::Assignment::RO, 'a bound value is returned bare';

my $c;
$c //= 5;
is $c, 5, '//= stores into an undefined variable';
my $d = 0;
$d ||= 7;
is $d, 7, '||= stores over a false value';
my $e = 1;
$e &&= 9;
is $e, 9, '&&= stores over a true value';

class A {
    has $!t;
    method t is rw { $!t //= "x" }
    method g { $!t }
}
my $o = A.new;
is $o.t, "x", 'an attribute //= in an rw method initialises it';
$o.t = "y";
is $o.g, "y", 'and the method returns the attribute container';

sub f($p) { ($p //= 1) += 1 }
dies-ok { f(3) }, 'a read-only parameter is not assignable through //=';
