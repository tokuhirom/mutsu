use v6;
use Test;

# A reduction whose operator is written as a bracketed `Callable` term:
#
#     sub f($a, $b) { $a + $b * 2 }
#     [[&f]]  1, 2, 3      # 11
#     [\[&f]] 1, 2, 3      # (1 5 11)
#
# The FOLD spelling already worked; the SCAN one did not parse, and neither did
# any other bracketed scan (`[\[+]]`, `[\R[+]]`) — `flatten_bracket_op` did not
# look past the `\` marker, so the inner bracket never reached it and the whole
# thing was rejected as an unknown reduction operator. Two runtime gaps came out
# with it: `R` over a bracketed callable (`[R[&f]]`), and a callable that IS a
# builtin-operator reference (`my &op = &[+]`).
#
# Note that `[&f] 1, 2, 3` — without the inner brackets — is a parse error in
# rakudo too: `[&f]` alone is an Array literal. The double-bracket spelling is
# the reduction. Every expectation below was measured against rakudo 2026.07.

plan 20;

sub f($a, $b) { $a + $b * 2 }
my &g = -> $a, $b { $a * 10 + $b };

# --- the ticket's repro -------------------------------------------------
is ([\[&f]] 1, 2, 3).join(' '), '1 5 11', 'the scan form over a bracketed Callable';
is ([[&f]] 1, 2, 3), 11, 'the fold form over a bracketed Callable';
is ([\[&g]] 1, 2, 3).join(' '), '1 12 123', 'the scan form over a `&`-sigil block';
is ([[&g]] 1, 2, 3), 123, 'the fold form over a `&`-sigil block';

# --- every other bracketed scan spelling, which broke the same way -------
is ([\[+]] 1, 2, 3).join(' '), '1 3 6', 'a bracketed builtin op in scan form';
is ([\[max]] 3, 1, 5).join(' '), '3 3 5', 'a word-form builtin op, bracketed';
is ([\R[+]] 5, 2, 1).join(' '), '1 3 8', 'a bracketed builtin op under R, in scan form';
is ([\+] 1, 2, 3).join(' '), '1 3 6', 'the un-bracketed scan still works';
is ([R[+]] 1, 2, 3), 6, 'the un-bracketed R fold still works';

# --- R over a bracketed callable ---------------------------------------
is ([R[&f]] 1, 2, 3), 9, 'R reverses a bracketed-Callable fold';
is ([\R[&f]] 1, 2, 3).join(' '), '3 7 9', 'and the scan form of it';

# --- a callable that is a builtin-operator reference --------------------
my &op = &[+];
is ([[&op]] 1, 2, 3), 6, 'a `&[+]` reference folds';
is ([\[&op]] 1, 2, 3).join(' '), '1 3 6', 'and scans';
my &mul = &infix:<*>;
is ([[&mul]] 2, 3, 4), 24, 'the `&infix:<*>` spelling too';

# --- arity, sources, laziness ------------------------------------------
sub three($a, $b, $c) { $a + $b + $c }
is ([[&three]] 1, 2, 3), 6, 'a 3-arity callable chunks by its arity';

my @a = 1, 2, 3;
is ([[&f]] @a), 11, 'an @-array operand';
is ([\[&f]] @a).join(' '), '1 5 11', 'an @-array operand, scan form';
is ([[&f]] @(1, 2, 3)), 11, 'an itemised list operand';

is ([\[&f]] (1..*))[^4].join(' '), '1 5 11 19', 'a lazy source stays pullable';
ok ([\[&f]] (1..*)).is-lazy, 'and stays lazy';
