use Test;

# Regression pin for docs/vm-dual-store.md Slice 4c part 2c: the closure exit
# path's per-call sigilless-alias / readonly / state-var writeback scans are
# gated behind a monotonic "any closure-writeback metadata key has been
# created" flag (env.rs CLOSURE_META_KEY_SEEN, set in Env::insert). The common
# closure (no sigilless vars, no state vars) must skip those scans, and the
# scans must still fire correctly once such metadata exists.

plan 13;

# --- Ordinary closures (flag may be false): basic capture/mutate still works.
sub make-counter() {
    my $n = 0;
    my $inc = -> { $n = $n + 1; $n };
    my @r;
    @r.push($inc()) for ^3;
    @r;
}
is make-counter(), [1, 2, 3], 'plain mutating closure captures/writes back';

# Read-only closure (the hot map/grep shape) unaffected.
my @doubled = (1, 2, 3).map({ $_ * 2 });
is @doubled, [2, 4, 6], 'read-only map block unaffected';

my @big = (1..5).grep({ $_ > 2 });
is @big, [3, 4, 5], 'read-only grep block unaffected';

# --- State variables (flips the meta flag): per-instance state persists.
my $bump = { state $s = 0; $s = $s + 1; $s };
is $bump(), 1, 'state var first call';
is $bump(), 2, 'state var second call';
is $bump(), 3, 'state var third call';

# Fresh closure instance from a factory gets fresh state.
sub make-state() { return { state $c = 0; $c = $c + 1; $c } }
my $a = make-state();
my $b = make-state();
is $a(), 1, 'instance A state starts fresh';
is $a(), 2, 'instance A state accumulates';
is $b(), 1, 'instance B state is independent';

# State array captured + mutated inside a closure.
my $collect = { state @seen; @seen.push($^x); @seen.elems };
is $collect(10), 1, 'state array push #1';
is $collect(20), 2, 'state array push #2';

# --- After metadata exists, ordinary closures still behave (flag is monotonic
# so the now-always-true flag must not corrupt non-metadata closures).
is make-counter(), [1, 2, 3], 'plain closure correct after meta flag is set';

# Nested closure writes an outer captured variable through a call.
sub outer() {
    my $acc = 0;
    my $add = -> $v { $acc = $acc + $v };
    my $apply = -> $f, $x { $f($x) };   # forwards a call to a mutating closure
    $apply($add, 5);
    $apply($add, 7);
    $acc;
}
is outer(), 12, 'nested closure mutation propagates through a forwarded call';
