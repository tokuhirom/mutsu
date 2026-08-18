use Test;

plan 3;

# A captured `&name` reference to a popped proto/multi import used to
# stack-overflow instead of dying. `my (&xrecur) = do { use
# ProtoRecursionFixture; (&xrecur) }` binds a name-based
# `Routine{package:"GLOBAL", name:"xrecur"}` value into the outer `&xrecur`
# local -- a proto/multi has no single candidate to point at, so it is a
# name reference, not a bound closure. Once the `do` block's import scope
# pops, `GLOBAL::xrecur` is removed from the proto tables (correctly).
# Calling `xrecur(...)` afterwards used to recurse forever: call_sub_value's
# unconditional call_function fallback re-dispatches "xrecur" by name,
# call_function_fallback's env-based callable lookup finds the SAME Routine
# value bound to the outer `&xrecur` local, and calls it again -- no base
# case, so it stack-overflowed instead of raising a catchable error.
# (`ProtoRecursionFixture`'s export name deliberately does not collide with
# any mutsu builtin/listop -- unlike e.g. `head` -- so the call is forced
# through the exact registry/env path this bug lives in.)
#
# The fix does not make this construct actually WORK (raku itself would
# have already reported "Undeclared routine" at compile time for the
# equivalent construct, since it resolves names lexically) -- it only
# ensures mutsu fails with a normal, catchable runtime error instead of
# crashing the process.

my $code = q:to/CODE/;
    my (&xrecur) = do {
        use lib 't/lib';
        use ProtoRecursionFixture;
        (&xrecur)
    };
    xrecur(1);
    CODE

my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
$proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);

isnt $proc.exitcode, 0, 'the construct still fails (not silently succeeding)';
# A crashed (aborted/segfaulted) child reports a negative `.exitcode` under
# mutsu's own `Proc` (confirmed via a direct repro: exitcode -1, stderr
# containing the Rust "has overflowed its stack" abort message) rather than
# a shell's usual 128+signal convention. A normal, catchable runtime error
# exits with mutsu's ordinary positive error status instead -- assert we
# are in that range, not the crash range, and that the crash's own
# telltale message is gone from stderr.
ok $proc.exitcode > 0,
    'fails with a normal (positive) error exit code, not a crashed/signal-killed process';
unlike $err, /'overflowed its stack' | 'fatal runtime error'/,
    'stderr does not contain the stack-overflow abort message';
