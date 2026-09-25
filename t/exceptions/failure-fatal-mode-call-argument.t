use Test;
use fatal;

# `use fatal` holds inside every routine declared under it, whatever its
# signature: a Failure passed as a call argument throws at the call site
# (#9453). A routine with parameters used to run through TRIR, whose call
# ops skipped the check.

sub mk($b) { $b ?? Failure.new("boom") !! 1 }
sub h($) { 1 }
sub hm($, $) { 1 }

sub none()          { h(mk(True)); "no-throw" }
sub one($b)         { h(mk($b)); "no-throw" }
sub several($a, $b, $c) { hm(mk($b), $c); "no-throw" }
sub typed(Int $n)   { h(mk($n > 0)); "no-throw" }

is (try { none() }) // "threw", "threw", 'a sub with no parameters';
is (try { one(True) }) // "threw", "threw", 'a sub with one parameter';
is (try { several(1, True, 3) }) // "threw", "threw", 'a sub with several parameters';
is (try { typed(5) }) // "threw", "threw", 'a sub with a typed parameter';
is one(False), "no-throw", 'no Failure, no throw';

class C {
    method m($b) { h(mk($b)); "no-throw" }
    method n($b) { self.take(mk($b)); "no-throw" }
    method take($) { 1 }
}
is (try { C.m(True) }) // "threw", "threw", 'a method with a parameter';
is (try { C.n(True) }) // "threw", "threw", 'a Failure passed to a method';

# The exception is the Failure's own.
throws-like { one(True) }, X::AdHoc, message => 'boom', 'the Failure itself is thrown';

done-testing;
