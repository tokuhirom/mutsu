use Test;

plan 2;

is infix:<Z>(<a b>, <c d>).gist, '((a c) (b d))',
    'the Z operator works in function-call form';
is (<a b> Z <c d>).gist, '((a c) (b d))',
    'the ordinary infix form remains unchanged';
