use Test;

# Declaring an operator with an empty (whitespace-only) symbol is a compile-time
# X::Syntax::Extension::Null ("Null operator is not allowed").

plan 4;

throws-like 'sub infix:sym< >() { }', X::Syntax::Extension::Null;
throws-like 'sub infix:< >() { }', X::Syntax::Extension::Null;

# Operators with a real symbol still declare and work.
sub infix:<foo>($a, $b) { $a ~ $b }
is ('x' foo 'y'), 'xy', 'infix:<foo> works';

sub circumfix:<[ ]>($x) { $x * 2 }
is [5], 10, 'circumfix:<[ ]> works';
