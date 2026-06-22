use Test;

# A `lazy { … }` thunk bound to a lexical must not deadlock the interpreter.
# The END-phaser overlay (run.rs) compares each captured lexical against the
# live env with `v != orig_v`; for a lexical holding a `Value::LazyThunk` both
# sides are the *same* Arc, so `PartialEq` locked the thunk's single non-reentrant
# `cache` mutex twice and the process hung on a futex at program exit. Comparing
# a thunk to itself must short-circuit on Arc pointer identity. Reaching the END
# phaser at all (this `use Test` registers one) is the regression trigger.

plan 4;

# A stored lazy thunk that is never forced must not hang at exit.
my $unforced := lazy { 42 };
ok $unforced.defined.so || !$unforced.defined.so, "unforced lazy thunk in scope is harmless";

# A stored lazy thunk that IS forced must still work and not hang.
my $was-run = 0;
my $forced := lazy { $was-run++; 99 };
is $forced, 99, "forced lazy thunk yields its value";

# Self-comparison of a lazy thunk short-circuits (does not deadlock).
my $t := lazy { 7 };
ok $t === $t, "a lazy thunk is identical to itself";

# A second independent thunk compares without deadlock.
my $a := lazy { 1 };
my $b := lazy { 2 };
ok !($a === $b), "two distinct lazy thunks are not identical";
