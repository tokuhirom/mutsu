use v6;
use Test;

# `$*THREAD` is the current thread's own Thread object: every read returns
# the same object, so a role mixed in with `$*THREAD does R` stays visible
# (LogP6 keeps its per-thread context this way).

plan 6;

ok $*THREAD === $*THREAD, '$*THREAD is one object per thread';

role Ctx { has $.ctx is rw = 'fresh' }

sub get-ctx() {
    return $*THREAD.ctx;
    CATCH { default { $*THREAD does Ctx; return $*THREAD.ctx } }
}

is get-ctx(), 'fresh', 'the first call mixes the role in';
$*THREAD.ctx = 'kept';
is get-ctx(), 'kept', 'the mixin persists on $*THREAD';

my $t = Thread.start({
    my $inner = $*THREAD;
    $inner does Ctx;
    $inner.ctx = 'inner';
});
$t.finish;
ok $t ~~ Ctx, '$*THREAD inside Thread.start is the Thread object it returned';
is $t.ctx, 'inner', 'and sees the mixin applied from inside';
is get-ctx(), 'kept', 'the main thread keeps its own object';
