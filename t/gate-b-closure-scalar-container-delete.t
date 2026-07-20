use v6;
use Test;

# (B)-gate regression pin: a nested closure whose ONLY use of an outer SCALAR
# is an in-place container mutation (`$bh<a>:delete`) never recorded that scalar
# as a free variable (element-delete is classified as a container mutation, and
# the container-write free-var set is filtered to `@`/`%` aggregates — a scalar
# holding a Bag/Hash/Array is neither). Under MUTSU_GATE_LOCAL_ENV_WRITE the
# outer `my $bh = <a a b>.BagHash` skips its env mirror, so when the closure ran
# by-name inside a carrier (`lives-ok { ... }`) the `:delete` read the decl-seed
# `Any` from env and the mutation vanished. Must pass identically with the gate
# OFF (default) and ON.

plan 8;

# BagHash held in a scalar, deleted inside a lives-ok carrier closure.
my $bh = <a a b>.BagHash;
lives-ok { $bh<a>:delete }, 'BagHash :delete in carrier lives';
is $bh<a>, 0, 'BagHash key removed through carrier closure';

# Plain Hash held in a scalar.
my $h = { a => 1, b => 2 };
lives-ok { $h<a>:delete }, 'scalar Hash :delete in carrier lives';
is $h.elems, 1, 'scalar Hash key removed through carrier closure';

# SetHash held in a scalar.
my $sh = <a b>.SetHash;
lives-ok { $sh<a>:delete }, 'SetHash :delete in carrier lives';
is $sh<a>, False, 'SetHash key removed through carrier closure';

# Array element delete held in a scalar, mutated inside a carrier closure.
my $a = [10, 20, 30];
lives-ok { $a[1]:delete }, 'scalar Array [i]:delete in carrier lives';
is $a[1]:exists, False, 'scalar Array element removed through carrier closure';
