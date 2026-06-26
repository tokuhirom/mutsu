use Test;

# The tight `&&` (Tight AND) binds TIGHTER than the conditional `?? !!`,
# so `$a && $b ?? $c !! $d` parses as `($a && $b) ?? $c !! $d`.
# Previously mutsu mis-parsed it as `$a && ($b ?? $c !! $d)`, so a false
# left operand short-circuited the whole expression to a Bool.

plan 15;

# Core regression: a false left operand must still reach the ternary.
my $e = True;
my $d = False;
is (!$e && !$d ?? "A" !! "B"), "B", 'false (a && b) selects the else branch';

is (True  && True  ?? 1 !! 2), 1, 'true && true picks then';
is (True  && False ?? 1 !! 2), 2, 'true && false picks else';
is (False && True  ?? 1 !! 2), 2, 'false && true picks else (no short-circuit leak)';
is (False && False ?? 1 !! 2), 2, 'false && false picks else';

# With comparison operands.
is (1 > 0 && 2 > 1 ?? "y" !! "n"), "y", 'comparisons under && then ternary';
is (1 > 0 && 2 < 1 ?? "y" !! "n"), "n", 'comparisons under && then ternary (else)';

# Tight `||` was already correct — keep it that way.
is (False || True  ?? 1 !! 2), 1, '|| stays tighter than ?? !!';
is (False || False ?? 1 !! 2), 2, '|| else branch';

# Loose word-logicals stay LOOSER than the ternary (they re-associate so the
# ternary binds first). The loose `and` shares no token with tight `&&` — it
# carries `AndWord` — so it must keep re-associating even though `&&` no longer does.
is (False and  True ?? "t" !! "e"), "False", 'loose and is looser than ?? !! (false short-circuits)';
is (True  and False ?? "t" !! "e"), "e",     'loose and is looser than ?? !! (else)';
is (True  and  True ?? "t" !! "e"), "t",     'loose and is looser than ?? !! (then)';
is (False andthen 1 ?? "a" !! "b"), "a", 'andthen is looser than ?? !!';
is (Nil orelse 5 ?? "x" !! "y"),     "x", 'orelse is looser than ?? !!';
is (0 or 1 ?? "p" !! "q"),           "p", 'or is looser than ?? !!';
