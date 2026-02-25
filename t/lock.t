use Test;

plan 9;

my $lock = Lock.new;
is $lock.WHAT, "(Lock)", "Lock.new returns a Lock";

$lock.lock;
pass "lock acquires lock";
$lock.lock;
pass "lock is re-entrant on same thread";
$lock.unlock;
$lock.unlock;
pass "unlock releases re-entrant lock";

my $ret = $lock.protect({
    123;
});
is $ret, 123, "protect executes block and returns its result";

my $cond = $lock.condition;
ok $cond.defined, "condition returns a condition variable";

my $async-lock = Lock::Async.new;
is $async-lock.WHAT, "(Lock::Async)", "Lock::Async.new returns a Lock::Async";

$lock.protect({
    my $ready = True;
    $cond.wait({ $ready });
});
pass "wait is callable with predicate";

$lock.protect({
    $cond.signal;
    $cond.signal_all;
});
pass "signal and signal_all are callable";
