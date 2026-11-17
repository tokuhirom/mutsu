use Test;
use nqp;

# `is raw` and sigilless method parameters bind through the method fast
# binder's container transport (ADR-15737 D1): a caller variable is promoted
# to its shared cell, a Proxy is relayed as itself, and a value binds as a
# read-only term. Every observable of the alias must match the general binder.

plan 29;

class A {
    has $.s;
    method raw(Mu $a is raw) { $a = 5 }
    method sig(Mu \a) { a = 6 }
    method keep($a is raw) { $!s := $a }
    method setk($v) { $!s = $v }
    method ro($a is raw) { $a = 1 }
    method rosig(\a) { a = 1 }
    method iscont(Mu $a is raw) { nqp::iscont($a) }
    method varname(Mu $a is raw) { $a.VAR.^name }
    method eqv($a is raw, $b is raw) { $a =:= $b }
    method inc(\a) { a++ }
    method typed(Int $a is raw) { $a = $a + 1 }
}

my $o = A.new;
my $x = 1; $o.raw($x);
is $x, 5, 'an `is raw` parameter writes the caller variable';
my $y = 1; $o.sig($y);
is $y, 6, 'a sigilless parameter writes the caller variable';
my $z = 1; $o.keep($z); $o.setk(9);
is $z, 9, 'binding an attribute to an `is raw` parameter aliases the caller variable';
throws-like { $o.ro(3) }, X::AdHoc, 'an `is raw` parameter bound to a value is immutable';
throws-like { $o.rosig(3) }, X::Assignment::RO, 'a sigilless parameter bound to a value is read-only';

my $w = 1;
is $o.iscont($w), 1, 'a variable argument arrives as a container';
is $o.iscont(3), 0, 'a value argument arrives as no container';
is $o.varname($w), 'Scalar', '.VAR of a variable argument is its Scalar';
is $o.varname(3), 'Int', '.VAR of a value argument is the value';
ok $o.eqv($w, $w), 'the same variable twice is the same container';
nok $o.eqv($w, $x), 'two variables are two containers';

my $c = 0; $o.inc($c) for ^3;
is $c, 3, 'repeated calls keep writing through the same container';
my @a = 1, 2; $o.raw(@a[0]);
is-deeply @a, [5, 2], 'an array element argument is written';
my %h = a => 1; $o.sig(%h<a>);
is %h<a>, 6, 'a hash element argument is written';

my $stored;
my $pp := Proxy.new(FETCH => -> $ { 42 }, STORE => -> $, $v { $stored = $v });
$o.raw($pp);
is $stored, 5, 'a bound Proxy argument stores through the Proxy';
is $o.varname($pp), 'Proxy', '.VAR of a bound Proxy argument is the Proxy';

class B { has $.v is rw = 1 }
my $b = B.new; $o.raw($b.v);
is $b.v, 5, 'an `is rw` accessor argument is written';

my $n = 1;
my &cl = { $o.raw($n) };
cl();
is $n, 5, 'a closure-captured outer variable is written';

my int $ni = 1; $o.raw($ni);
is $ni, 5, 'a native int variable is written';

my $typed = 1; $o.typed($typed);
is $typed, 2, 'a typed `is raw` parameter reads and writes the caller variable';
throws-like { $o.typed('x') }, X::TypeCheck::Binding::Parameter,
    'a typed `is raw` parameter still checks its type';

class C is A {
    has $.t = 1;
    method relay(Mu $a is raw) { self.raw($a) }
    method relaysig(\a) { self.sig(a) }
    method viaattr { self.raw($!t); $!t }
    method raw(Mu $a is raw) { callsame; $a = $a + 100 }
}
my $cc = C.new;
my $r1 = 1; $cc.relay($r1);
is $r1, 105, 'an `is raw` parameter relayed to another `is raw` call writes the original';
my $r2 = 1; $cc.relaysig($r2);
is $r2, 6, 'a sigilless parameter relayed to another sigilless call writes the original';
is $cc.viaattr, 105, 'an attribute argument is written';
my $r3 = 1; $cc.raw($r3);
is $r3, 105, 'callsame hands the container on';

sub tw(\a) { a = 11 }
my $r4 = 1; $o.raw($r4); tw($r4);
is $r4, 11, 'a promoted variable keeps working with a sub alias';
my $u; $o.raw($u);
is $u, 5, 'an undefined variable is written';
my Int $ti = 1; $o.raw($ti);
is $ti, 5, 'a typed variable is written';
throws-like { my Int $tj = 1; $o.raw($tj); $o.sig($tj); my Str $ts = 'a'; $o.raw($ts) },
    X::TypeCheck::Assignment, "the caller variable's own type constraint still applies";
