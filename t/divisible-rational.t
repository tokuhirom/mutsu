use Test;

# infix:<%%> ("is divisible by") is defined as `$a % $b == 0` and must work for
# Rat/Num/BigInt operands, not only Int. Previously mutsu returned False for any
# non-Int operand.

plan 24;

# Rat operands
ok   3.5 %% 0.5,  "3.5 %% 0.5 is True (7 times)";
nok  3.6 %% 0.5,  "3.6 %% 0.5 is False";
ok   1.0 %% 0.5,  "1.0 %% 0.5 is True";
ok   0.5 %% 0.5,  "0.5 %% 0.5 is True";
ok   0.3 %% 0.1,  "0.3 %% 0.1 is True";
ok   10  %% 2.5,  "Int %% Rat works (10 %% 2.5)";
ok   7   %% 0.5,  "7 %% 0.5 is True";

# Negative operands (divisor-sign / floor semantics, same as %)
ok  -6   %% 2,    "-6 %% 2 is True";
ok   6   %% -2,   "6 %% -2 is True";
ok  -6.5 %% 0.5,  "-6.5 %% 0.5 is True";
ok  -3.5 %% 0.5,  "-3.5 %% 0.5 is True";

# Plain Int still works
ok   6 %% 2,  "6 %% 2 is True";
nok  7 %% 2,  "7 %% 2 is False";

# BigInt operands
nok (2 ** 70)     %% 5, "2**70 %% 5 is False";
ok  (2 ** 70 + 1) %% 5, "2**70+1 %% 5 is True";

# String operands coerce numerically
ok  "6" %% 2, '"6" %% 2 is True';

# FatRat operands
ok FatRat.new(7, 2) %% 0.5, "FatRat %% Rat works";

# !%% (not divisible)
nok  6.6 !%% 2.2, "6.6 !%% 2.2 is False (divisible)";
ok   6.5 !%% 2.2, "6.5 !%% 2.2 is True (not divisible)";
ok   7   !%% 2,   "7 !%% 2 is True";
nok  6   !%% 2,   "6 !%% 2 is False";

# Divide-by-zero throws, reporting infix:<%%>
throws-like { 7   %% 0   }, X::Numeric::DivideByZero, "Int %% 0 throws";
throws-like { 3.5 %% 0   }, X::Numeric::DivideByZero, "Rat %% 0 throws";
throws-like { 7   %% 0.0 }, X::Numeric::DivideByZero, "Int %% 0.0 throws";

done-testing;
