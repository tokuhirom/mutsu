use lib 't/lib';
use Test;

# The declared key type of a `module`/`package` block's `my %seen{Any}` stays
# with the block: a unit file declaring a same-named `my %seen{Hash}` no longer
# makes the block's routines check their stores against `Hash`. The types of a
# block's `@`/`%` lexicals live in name-keyed env lanes that the block left
# behind in the enclosing env (PDF 0.6.17's `PDF::COS::Util` next to
# `PDF::COS::Dict`). Expected values are the `raku` (v2026.09) output.

use TypedHashLaneUnit;
use TypedHashLaneBlock;

plan 4;

is TypedHashLaneUnit.put({ a => 1 }), 1, 'the unit file stores a Hash key';
is put-any((1, 2)), 1, 'the block stores a List key into its %seen{Any}';
is put-any({ c => 1 }), 2, 'and a Hash key';
is TypedHashLaneUnit.put({ b => 2 }), 2, 'the unit file still checks against Hash';
