use Test;

plan 9;

# A ternary `?? !!` is at conditional precedence, which is tighter than the
# comma that separates no-paren call arguments. So `f $cond ?? $a !! $b` must
# bind the ternary *inside* the argument, not wrap the whole call as
# `(f $cond) ?? $a !! $b`.

sub one($v) { $v }
sub two($a, $b) { "$a|$b" }

is (one True ?? "yes" !! "no"), "yes", 'ternary binds inside a no-paren arg';
is (one False ?? "yes" !! "no"), "no", 'ternary false branch inside a no-paren arg';

# Comparison condition inside the argument.
is (one 5 > 3 ?? "big" !! "small"), "big", 'comparison ternary cond in arg';

# Nested ternary inside the argument.
is (one 1 ?? (0 ?? "a" !! "b") !! "c"), "b", 'nested ternary in arg';

# `&&` / `||` are tighter than the ternary, so they belong to the condition.
is (one True && False ?? "T" !! "F"), "F", '&& binds tighter than ?? in arg';
is (one True || False ?? "T" !! "F"), "T", '|| binds tighter than ?? in arg';

# Item assignment is looser than the ternary: `$x = cond ?? a !! b`.
my $x;
is (one $x = 2 > 1 ?? "P" !! "Q"), "P", 'assignment RHS parses a ternary in arg';
is $x, "P", 'the assignment took effect with the ternary value';

# Multiple arguments each carrying a ternary.
is (two 1 ?? "x" !! "y", 2 ?? "m" !! "n"), "x|m", 'two args each with a ternary';
