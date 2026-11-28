# The fast accessor read memoizes its gate per (class, method) (#19293).
# Every gate input lives in the registry, so a declaration made after the
# accessor is hot -- a wrap, an augment, a subclass override -- must still
# be honoured on the next read. Each read runs in a loop so the memo is warm.
use Test;

plan 9;

class P { has $.x; has $.y is rw; }
my $p = P.new(x => 3, y => 4);
my $s = 0;
$s += $p.x for ^200;
is $s, 600, 'a plain accessor read';

class Q is P { method x { 10 } }
my $q = Q.new(x => 3);
$s = 0;
$s += $q.x for ^200;
is $s, 2000, 'a subclass method beats the inherited accessor';
$s = 0;
$s += $p.x for ^200;
is $s, 600, 'the parent class still reads its attribute';

my $wrapped = P.^find_method('x').wrap(-> |c { callsame() * 2 });
$s = 0;
$s += $p.x for ^200;
is $s, 1200, 'a wrap added after the accessor was hot is honoured';
$wrapped.restore;
$s = 0;
$s += $p.x for ^200;
is $s, 600, 'and so is restoring it';

use MONKEY-TYPING;
class R { has $.v = 1 }
my $r = R.new;
$s = 0;
$s += $r.v for ^200;
is $s, 200, 'before augment';
augment class R { method w { 7 } }
$s = 0;
$s += $r.v + $r.w for ^200;
is $s, 1600, 'after an augment of the class';

$p.y = 9 for ^5;
is $p.y, 9, 'an rw accessor still assigns';
class T { has $!secret = 5; method peek { $!secret } }
throws-like { T.new.secret }, X::Method::NotFound, 'a private attribute stays private';
