use Test;

# A BEGIN block runs at compile time, so it can only see routines declared
# *before* it. Calling a sub declared *later* in the same unit is
# X::Undeclared::Symbols at BEGIN time, even though the sub exists by unit end.

plan 5;

throws-like 'BEGIN { ohnoes() }; sub ohnoes() { }', X::Undeclared::Symbols,
    'BEGIN calling a sub declared later is X::Undeclared::Symbols';
throws-like 'BEGIN { my $x = laterfn() + 1 }; sub laterfn() { 2 }', X::Undeclared::Symbols,
    'forward call nested in an expression is detected';

# Valid: BEGIN calling an earlier-declared sub, or a builtin.
lives-ok { EVAL 'sub okfn() { 42 }; BEGIN { okfn() }' },
    'BEGIN calling an earlier-declared sub lives';
lives-ok { EVAL 'BEGIN { my $x = 1 + 1 }' },
    'BEGIN with no user calls lives';
lives-ok { EVAL 'BEGIN { say "hi" }' },
    'BEGIN calling a builtin lives';
