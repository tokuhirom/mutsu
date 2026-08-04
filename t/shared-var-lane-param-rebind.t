use v6;
use Test;

plan 4;

# The name-keyed @/% shared-var lane is seeded once per name (seed_if_absent),
# so a name re-bound per invocation through the env-level parameter binding
# path froze at the FIRST spawn's binding: every later spawned block read the
# first binding's value (todo/tickets/shared-var-lane-freezes-a-reused-array-name.md).

# A runtime-invoked callback's plain @ parameter, re-bound per reduce
# iteration and captured by a start block. Was 6 (3 + 3: the second
# iteration's start saw the first iteration's @words).
is (reduce -> $h, @words { $h + await start { [+] @words } }, 0, (1, 2), (3, 4)), 10,
    'reduce callback @ param is fresh per iteration inside start';

# The %-sigil twin of the same shape.
is (reduce -> $h, %w { $h + await start { [+] %w.values } }, 0, %(a => 1, b => 2), %(c => 3, d => 4)), 10,
    'reduce callback % param is fresh per iteration inside start';

# A destructured @ param must stay off the lane even at the FIRST spawn in the
# process (the recording used to be gated on shared_vars_active, which is still
# false at that spawn), so a later plain binding of the same name is not
# poisoned by a frozen lane entry. Order matters: this pair must run in this
# sequence in one process. Was 100|100 on the second line.
is (await map -> [$a, @K] { start { "$a:{@K[0]}" } }, (1, (100, 101)), (2, (200, 201))).join('|'),
    '1:100|2:200', 'destructured @ param correct per spawn';
is (await map -> @K { start { @K[0] } }, (300, 301), (400, 401)).join('|'),
    '300|400', 'plain @ param reusing the destructured name is not frozen';
