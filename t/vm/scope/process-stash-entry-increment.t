use Test;

# `PROCESS::<$x>++` (and `--`, prefix or postfix) on a key that does not exist
# yet creates the dynamic variable, as `PROCESS::<$x> += 1` already did; the
# increment used to go into a snapshot of the stash (#18120).

plan 7;

PROCESS::<$INCR-A>++;
is $*INCR-A, 1, 'postfix ++ creates the dynamic variable';
PROCESS::<$INCR-A>++;
is $*INCR-A, 2, '... and increments it in place';
is PROCESS::<$INCR-B>++, 0, 'postfix ++ on an absent entry yields 0';
is $*INCR-B, 1, '... and stores the incremented one';
is ++PROCESS::<$INCR-C>, 1, 'prefix ++ yields the new value';
PROCESS::<$INCR-C>--;
is $*INCR-C, 0, 'postfix -- writes back';
is --PROCESS::<$INCR-C>, -1, 'prefix -- yields the new value';
