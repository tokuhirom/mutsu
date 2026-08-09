use MONKEY-SEE-NO-EVAL;
use Test;
plan 6;

# Raku's `...` stub uses fail() semantics (not die()):
# - Calling a stub routine returns Failure to the caller
# - try { ... } still catches it (Failure on stack is caught by try)
# - Failure in sink context (e.g. sunk lazy Seq) propagates as is_fail,
#   which at a sub boundary converts to Failure for the caller

# 1-2: stub sub returns Failure to caller
sub stub-sub() { ... }
my $r = stub-sub();
ok !defined($r), 'stub sub return is not defined';
ok $r ~~ Failure, 'stub sub returns a Failure';

# 3: try { ... } still catches the stub (fail propagates to try like die)
try { ... }
ok $! ~~ X::StubCode, 'try catches stub, $! is X::StubCode';

# 4-5: eval_exception pattern (real Test.rakumod usage)
# EVAL q[map -> $x,$y { ... }, 1..6] returns a lazy Seq;
# when that Seq is sunk outside try{}, the stub fires as fail(),
# which propagates through the sub boundary as Failure.
sub eval_exception_test($code) {
    try { EVAL ($code) }
    $!
}
my $ee = eval_exception_test(q[map -> $x, $y { ... }, 1..6]);
ok !defined($ee), 'eval_exception returns not-defined for lazy stub map';
ok $ee ~~ Failure, 'eval_exception returns Failure for lazy stub map';

# 6: stub class method returns Failure
class Foo { method bar() { ... } }
ok !defined(Foo.new.bar), 'stub class method returns not-defined Failure';
