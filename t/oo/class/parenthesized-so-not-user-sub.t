use Test;

# A user routine named `so` or `not` wins for the tight parenthesized call
# spelling, while the whitespace form remains the builtin unary operator.
plan 6;

sub so($value) { 42 } #OK shadow
sub not($value) { 42 } #OK shadow

is so(1), 42, 'parenthesized so() calls the user sub';
is so 1, True, 'spaced so remains the builtin unary operator';
is &so(1), 42, '&so() calls the user sub';

is not(1), 42, 'parenthesized not() calls the user sub';
is not 1, False, 'spaced not remains the builtin unary operator';
is &not(1), 42, '&not() calls the user sub';
