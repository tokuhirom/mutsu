use Test;

# Deep recursion must RAISE, not abort the process (ADR-0100, #8232).
#
# mutsu runs a Raku call as a Rust call, so Raku recursion is native
# recursion. Before the ADR-0100 guard, running out of native stack hit the
# thread's guard page and the Rust runtime killed the process: no unwinding,
# nothing for `try` to catch, no END phaser, no backtrace, and the rest of a
# test file simply never ran.
#
# Nothing here asserts a DEPTH. The depth the guard fires at is a property of
# the build -- a debug frame is several times larger than a release one, and a
# JIT-compiled frame is smaller again (measured: ~5,700 frames debug+JIT,
# ~1,500 debug without) -- so a depth assertion would be a flake waiting to
# happen. What is invariant is that the failure is catchable and that
# execution continues afterwards.

plan 7;

my $depth = 0;
sub runaway($n) { $depth = $n; runaway($n + 1) }

my $err;
{
    runaway(1);
    CATCH { default { $err = $_ } }
}

ok $err.defined, 'runaway recursion raises instead of aborting the process';
ok $err ~~ Exception, 'what it raises is an ordinary Exception';
like $err.message, /recursion/, 'whose message says it was the recursion';
ok $depth > 100, "and it got a useful way down first ($depth frames)";
pass 'execution continues after the raise';

# `try` contains it the same way, which is the shape a parser over
# attacker-controlled input depends on (JSON::Fast's t/01-parse.t feeds its
# parser a 10,000-deep document precisely to check this is survivable).
my $r = try { runaway(1); 'no error' };
nok $r.defined, 'try {} contains it too';

# The guard is per-call, not a one-shot latch: after unwinding back to a
# shallow frame the stack is free again and ordinary calls keep working.
sub twice($n) { $n * 2 }
is twice(21), 42, 'ordinary calls still work after the guard has fired';
