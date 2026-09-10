use Test;

# X/Z meta-operator Whatever-currying.
# A standalone `*` operand of an X/Z meta-op makes a WhateverCode; a trailing
# `*` in a comma-list operand is a list extender, not a curry placeholder.

plan 19;

# --- single standalone * curries (X) ---
{
    my $f = (1 X+ * X+ 3);
    isa-ok $f, Code, '1 X+ * X+ 3 curries';
    is $f(2), 6, '... applied (2 -> 6)';
    is $f(-4), 0, '... applied (-4 -> 0)';
}

# --- two standalone * curry to a 2-arg WhateverCode (X) ---
{
    my $f = (* X+ *);
    isa-ok $f, Code, '* X+ * curries (2 args)';
    is $f(41, 43), 84, '... applied';
    is $f(-1, 1), 0, '... applied';
}

# --- list operands with standalone * middle operand (X cross) ---
{
    my $f = (1, 2 X~ * X~ 3, 4);
    isa-ok $f, Code, '1,2 X~ * X~ 3,4 curries';
    is $f(<a b>).join(' '), '1a3 1a4 1b3 1b4 2a3 2a4 2b3 2b4', '... cross product';
}

# --- single standalone * curries (Z) ---
{
    my $f = (1 Z+ * Z+ 3);
    isa-ok $f, Code, '1 Z+ * Z+ 3 curries';
    is $f(2), 6, '... applied (2 -> 6)';
    is $f(-4), 0, '... applied (-4 -> 0)';
}

# --- two standalone * curry to a 2-arg WhateverCode (Z) ---
{
    my $f = (* Z+ *);
    isa-ok $f, Code, '* Z+ * curries (2 args)';
    is $f(41, 43), 84, '... applied';
}

# --- list operands with standalone * middle operand (Z zip) ---
{
    my $f = (1, 2 Z~ * Z~ 3, 4);
    isa-ok $f, Code, '1,2 Z~ * Z~ 3,4 curries';
    is $f(<a b>).join(' '), '1a3 2b4', '... zip';
}

# --- EXTEND: a trailing * in a comma-list operand is NOT a curry ---
is (1, 2, 3, * Z 10, 20, 30, 40, 50).join('|'),
    '1 10|2 20|3 30|3 40|3 50', 'trailing * extends left zip argument';
is (1, 2, 3, * Z+ 10, 20, 30, 40, 50).join(' '),
    '11 22 33 43 53', 'trailing * extends (Z+)';

# --- plain X/Z chains (no Whatever) keep working, including >2 operands ---
is (1, 2 X~ 3, 4 X~ 5, 6).join(' '),
    '135 136 145 146 235 236 245 246', 'X~ chain of three list operands';
is (1, 2 Z~ 3, 4 Z~ 5, 6).join(' '), '135 246', 'Z~ chain of three list operands';
