# A callee's `my \h = k` bind pair no longer leaks into its caller

A sigilless declaration bound to another local (`my \h = k`) records a slot
pair so that a write to one name reaches the other. The pair holds slot
indices of the declaring routine's frame, but the positional-light, typed-light
and fast call paths did not save and restore the pair list around the call, so
every call left one pair behind in the caller. Two things followed: the
caller's own slots with those indices became bound to each other (`my $x = 1;
my $y = 2; f(); $x = 10; say $y` printed `10`), and every later store scanned
one stale pair per earlier call, which made a loop calling such a routine
quadratic (64,000 calls took 6.2 s instead of 0.15 s). The pair list is now
per-frame on those paths as it already was on the general one, and a
declaration re-run in a loop body records its pair once.
