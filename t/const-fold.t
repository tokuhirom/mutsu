use v6;
use Test;

# Constant folding (ADR-0006 §2.1): literal-only pure expressions are evaluated
# at compile time. Every case here must keep the exact runtime semantics — the
# folded value has to be indistinguishable from the value the VM would compute.

plan 26;

# --- integer arithmetic ---------------------------------------------------
is 60 * 60 * 24, 86400, 'nested Int multiplication folds';
is 2 + 3 * 4, 14, 'precedence is preserved when folding';
is (2 + 3) * 4, 20, 'parenthesised subexpression folds';
is 7 - 9, -2, 'subtraction folds to a negative Int';
is 7 % 3, 1, 'modulo folds';
is -1 + 2, 1, 'unary minus operand folds';
is -(2 * 3), -6, 'unary minus of a folded subexpression';
is 10 - -3, 13, 'subtraction of a negative literal';

# --- type-preserving numeric promotion ------------------------------------
# The fold calls the VM's own arithmetic, so Int/Int stays a Rat and an
# overflowing power promotes to a big integer, exactly as at runtime.
is (1 / 3).WHAT.^name, 'Rat', 'Int / Int folds to a Rat, not a Num';
is 1 / 4, 0.25, 'Rat value is correct';
is (1 / 3) + (1 / 6), 0.5, 'Rat arithmetic folds exactly';
is 2 ** 100, 1267650600228229401496703205376, 'Int ** Int promotes to a big integer';
is (2 ** 100).WHAT.^name, 'Int', 'the promoted value is still an Int';
is 1.5 + 2.5, 4, 'Rat literals fold';
is (1.5 + 2.5).WHAT.^name, 'Rat', 'Rat + Rat stays a Rat';
is 1e2 + 1, 101e0, 'Num literal folds';
is (1e2 + 1).WHAT.^name, 'Num', 'Num + Int stays a Num';

# --- strings and comparisons ----------------------------------------------
is 'a' ~ 'b' ~ 'c', 'abc', 'string concatenation folds';
is 'x' ~ 1 ~ 2.5, 'x12.5', 'concatenation stringifies numeric literals';
ok 1 < 2, 'numeric comparison folds';
nok 2.0e0 > 3.0e0, 'Num comparison folds';
ok 1 == 1, 'equality folds';

# --- expressions that must NOT fold ---------------------------------------
my $x = 10;
is $x + 2, 12, 'an expression with a variable is not folded';
is '3' + 4, 7, 'string operand keeps the numeric-coercion path';
dies-ok { EVAL '1 div 0' }, 'a compile-time-evaluable error still dies at runtime';
throws-like { '3x' + 1 }, X::Str::Numeric, 'invalid numeric string still throws';

done-testing;
