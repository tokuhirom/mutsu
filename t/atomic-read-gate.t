use Test;

# Regression pin for docs/vm-dual-store.md Slice 4c part 2: the per-read atomic
# variable check in GetGlobal/GetLocal is gated behind a monotonic
# "an atomic var has been registered" flag, so ordinary variable reads no longer
# pay the atomic-name `format!` + constraint lookups. This must not change
# observable atomic semantics, and ordinary (non-atomic) reads must still work.

plan 10;

# Ordinary scalar reads (the gated-off common case) are unaffected.
my $x = 10;
my $y = $x + 5;
is $y, 15, 'ordinary scalar read unaffected by the gate';

# Reads inside a closure (free-variable GetGlobal — the hot path the gate
# optimizes) still resolve.
my $base = 100;
my $f = -> $n { $n + $base };
is $f(7), 107, 'closure free-variable read unaffected';

# Atomic scalar: increment / add / fetch still work once an atomic is declared
# (declaration flips the gate on).
my atomicint $a = 0;
$a⚛++;
$a⚛++;
is $a, 2, 'atomicint post-increment works';
is atomic-fetch-inc($a), 2, 'atomic-fetch-inc returns value before increment';
is $a, 3, 'atomic-fetch-inc incremented the value';
atomic-assign($a, 13);
is $a, 13, 'atomic-assign works';
is atomic-fetch($a), 13, 'atomic-fetch returns current value';

# Ordinary variables declared after an atomic still read correctly (the gate is
# monotonic, so the check now runs for every read but must not misfire on a
# non-atomic name).
my $z = 42;
is $z, 42, 'non-atomic read correct after an atomic exists';
is $f(1), 101, 'closure still correct after an atomic exists';

# Atomic shared across threads (the cross-thread shared-storage check path).
my atomicint $counter = 0;
my @p = (^4).map({ start { $counter⚛++ for ^250 } });
await @p;
is $counter, 1000, 'atomic counter correct across threads';
