use v6;
use Test;

# Regression: `Promise(supply { whenever $promise { ... } })` drives the
# subscription on a background thread (`supply_promise_on_demand`, so the
# caller's immediately-returned Planned promise never blocks). That thread
# runs the whenever body as real VM bytecode -- method dispatch, grammar/regex
# recursion, ordinary sub recursion, etc. -- not GC-helper plumbing, so it
# must get the large 256 MiB user-code stack (see `thread-deep-stack.t`), not
# the ~2 MiB default. Previously it was spawned with `spawn_gc_helper_thread`
# and deep recursion inside the whenever body overflowed the default stack
# and crashed the process with SIGSEGV (observed via Cro::HTTP::Cookie's
# grammar-driven regex parsing, https://github.com/tokuhirom/mutsu, ticket
# http-session-tests-crash-rc139-on-main).
#
# Depth ~600 overflows a default thread stack but fits comfortably in the
# 256 MiB user-code stack.

plan 1;

sub deep(Int $n) {
    return 0 if $n <= 0;
    return 1 + deep($n - 1);
}

my $p = Promise.new;
$p.keep(42);

my $result = await supply {
    whenever $p -> $v {
        emit deep(600) + $v;
    }
}.Promise;

is $result, 642, 'deep recursion in an on-demand supply Promise whenever body does not overflow the drive thread stack';
