use Test;

# An uncaught X::Numeric::DivideByZero prints its descriptive message (matching
# Rakudo), and integer `%` by zero reports the dividend and `using %`.
plan 8;

# `.message` carries the full description, not the bare type name.
{
    my $msg;
    { (7 % 0).sink; CATCH { default { $msg = .message } } }
    is $msg, 'Attempt to divide 7 by zero using %', 'int % 0 message';
}
{
    my $msg;
    { (1 div 0).sink; CATCH { default { $msg = .message } } }
    is $msg, 'Attempt to divide 1 by zero using div', 'int div 0 message';
}

# The exception type is correct.
{
    my $name;
    { (5 % 0).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Numeric::DivideByZero', '% by zero is X::Numeric::DivideByZero';
}

# Large (BigInt) dividend is rendered in the message.
{
    my $msg;
    { ((2 ** 70) % 0).sink; CATCH { default { $msg = .message } } }
    is $msg, 'Attempt to divide 1180591620717411303424 by zero using %',
        'BigInt % 0 message';
}

# `%%` (is-divisible) by zero keeps its own operator name.
{
    my $msg;
    { (7 %% 0).sink; CATCH { default { $msg = .message } } }
    is $msg, 'Attempt to divide 7 by zero using infix:<%%>', '%% by zero message';
}

# Both operands negative / mixed signs still report the dividend.
{
    my $msg;
    { (-8 % 0).sink; CATCH { default { $msg = .message } } }
    is $msg, 'Attempt to divide -8 by zero using %', 'negative dividend % 0';
}

# div by zero still throws the right type.
{
    my $name;
    { (9 div 0).sink; CATCH { default { $name = .^name } } }
    is $name, 'X::Numeric::DivideByZero', 'div by zero type';
}

# A valid modulo still works (no false positive).
is 7 % 3, 1, 'valid modulo still works';
