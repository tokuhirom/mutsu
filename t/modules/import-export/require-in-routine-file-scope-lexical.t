use lib 't/lib';
use Test;

# A module loaded by a `require` that runs inside a routine is still compiled
# as its own compilation unit: its top-level subs read its file-scope `my`
# variables, not a per-activation alias of the routine the `require` happened
# to run in (#18017: Monad::Maybe's lazy `require ::('Monad::Either')` saw
# Either's `my $type-cache` as Any).

plan 10;

use RequireInRoutineLexA;
my \B = RequireInRoutineLexA.b;
is B.plain, 42, 'a top-level sub reads the file-scope lexical';
is B.publish, 42, '... also from a Lock.protect block';
is B.seen-keys, 'a,b', 'a hash initializer copies the file-scope hash';
is B.bump, 43, 'a write through a protect block reaches the same variable';
is B.plain, 43, '... which every sub of the module sees';
isa-ok RequireInRoutineLexA.own-cache, Map, "the loader's same-named lexical is untouched";

# Reached through a punned role, read as an invocant, through a binding and as
# the block's value inside `Lock.protect` (#18126).
sub role-type { require ::('RequireInRoutineLexRole') }
my \R = role-type();
is R.p1, 'Map', 'a protect block reads it as a method invocant';
is R.p2, 'Map', '... through a copy';
is R.p3, 'Map', '... as the block value';
is R.p4, 'Map', '... and so does the sub itself';
