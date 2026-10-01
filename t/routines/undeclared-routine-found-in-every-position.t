use Test;

# The CHECK-time undeclared-routine walk is the typed AST visitor (ADR-0137):
# it descends into every child, so a call to an undeclared routine is found
# wherever it sits, and only real declarations suppress it.

plan 3;

throws-like 'my $x = 1; $x += nosuchroutine(); $x', X::Undeclared::Symbols,
    'a call on the right of a compound assignment';
throws-like 'my %h = { a => nosuchroutine() }; %h', X::Undeclared::Symbols,
    'a call in a hash-literal value';
lives-ok { EVAL 'my sub there() { 1 }; my $x = 0; $x += there(); $x' },
    'a declared routine in the same position is fine';
