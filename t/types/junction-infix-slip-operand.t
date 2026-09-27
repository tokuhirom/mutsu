use Test;

plan 8;

# The infix junction operators take `+values`, whose slurpy slips a Slip
# operand into the eigenstates: `|(1, 2) | 3` is `any(1, 2, 3)`.
# From URI::Query::FromHash, which builds its safe-byte set as
# `| 0x2D | 0x2E | (0x30..0x39) ...` (a leading prefix `|`).

is (|(1, 2) | 3).raku, 'any(1, 2, 3)', 'Slip on the left of |';
is (1 | |(2, 3)).raku, 'any(1, 2, 3)', 'Slip on the right of |';
is (|(1, 2) & 3).raku, 'all(1, 2, 3)', 'Slip operand of &';
is (|(1, 2) ^ 3).raku, 'one(1, 2, 3)', 'Slip operand of ^';
is (|(1, 2) | 3 | 4).raku, 'any(1, 2, 3, 4)', 'Slip operand in a list-associative chain';

my $safe = | 0x2D | 0x2E | (0x30..0x39);
ok 0x2D ~~ $safe, 'the slipped first operand is an eigenstate';
ok 0x35 ~~ $safe, 'a Range operand still smartmatches';

is (1 | 2 | 3).raku, 'any(1, 2, 3)', 'no Slip: unchanged';
