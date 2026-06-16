# `skip` collides with two routines: the core list routine
# `skip(Int $n, *@list)` and Test's `skip($reason?, $count = 1)`. With Test
# loaded, a numeric first arg + a list operand must dispatch to the core list
# routine (not be hijacked as a test-skip), while a Str first arg stays Test's
# skip. Regression for the VM fast-path that hijacked `skip` unconditionally.
use Test;

plan 6;

# Core list skip (numeric first arg) — must NOT emit SKIP TAP lines.
is-deeply skip(2, <a b c d e>), <c d e>, 'skip(Int, List) is the core list routine';
is-deeply skip(0, <a b c>),     <a b c>, 'skip(0, List) skips nothing';
is-deeply skip(5, <a b c>),     (),      'skip(N, List) past the end is empty';

my @array = <x y z w>;
is-deeply skip(2, @array), <z w>, 'skip(Int, @array) flattens the array operand';

# Method form is unaffected.
is-deeply <a b c d>.skip(1), <b c d>, 'List.skip(N) method form works';

# Test's skip (Str first arg) still skips tests.
skip "deliberately skipped", 1;
