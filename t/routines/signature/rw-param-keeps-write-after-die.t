use Test;
use nqp;

# Found via JSON::Native / JSON::Fast: nom-ws's `try nom-ws($text, $pos)` relies
# on a write to an `int $pos is rw` parameter surviving the sub dying afterwards
# (rakudo aliases the caller's container). The natively compiled call lost the
# write when the body threw.

plan 10;

sub a1(int $x is rw) { $x = 7; die "boom" }
my int $m = 1;
try a1($m);
is $m, 7, 'native int rw param keeps the write when the body dies';

my int $n = 1;
try { a1($n) }
is $n, 7, 'same inside a try block';

sub a2(Int $x is rw) { $x = 7; die "boom" }
my Int $k = 1;
try a2($k);
is $k, 7, 'Int rw param (control)';

sub deeper(int $p is rw) { $p = $p + 10; die "deep" }
sub nest(int $p is rw) { $p = $p + 1; deeper($p); }
my int $c = 0;
try nest($c);
is $c, 11, 'a chain of rw calls, callee declared first';

sub nest-fwd(int $p is rw) { $p = $p + 1; deeper-fwd($p); }
sub deeper-fwd(int $p is rw) { $p = $p + 10; die "deep" }
my int $d = 0;
try nest-fwd($d);
is $d, 11, 'a chain of rw calls, callee declared after the caller';

sub inner(str $t, int $pos is rw --> Nil) { $pos = nqp::add_i($pos, 5); die "inner" }
sub outer(str $t, int $pos is rw --> Nil) { inner($t, $pos = nqp::add_i($pos, 1)) }
my int $p = 0;
try outer("x", $p);
is $p, 6, 'an assignment expression as the rw argument';

sub nom(str $t, int $pos is rw --> Nil) { $pos = nqp::add_i($pos, 1); die "x" }
my int $f = 0;
try nom("a", $f);
is $f, 1, 'two parameters, second is rw';

sub ok-call(int $x is rw) { $x = 9 }
my int $g = 1;
ok-call($g);
is $g, 9, 'success path unchanged';

my int $h = 3;
my $r = try { a1($h); 5 };
is $h, 7, 'write kept when the try value is discarded';
nok $r.defined, 'and the try yields Nil';
