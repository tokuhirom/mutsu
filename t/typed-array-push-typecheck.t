use v6;
use Test;

plan 9;

# Pushing an element that violates a typed array's element type reports
# "for an element of @a" (like rakudo and the other array mutators), NOT the
# scalar "in assignment to @a" wording.

# .push
{
    my Int @a;
    my $ex;
    { @a.push("x"); CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::TypeCheck::Assignment, 'push type error is X::TypeCheck::Assignment';
    is $ex.message, 'Type check failed for an element of @a; expected Int but got Str ("x")',
        'push message names the element and the offending value';
    is $ex.got, "x", '.got is the offending value';
}

# A valid push still works
{
    my Int @a;
    @a.push(5);
    @a.push(6);
    is @a, [5, 6], 'valid pushes succeed';
}

# push matches append/unshift wording (all element mutators agree)
for <push append unshift prepend> -> $meth {
    my Int @a;
    my $ex;
    { @a."$meth"("x"); CATCH { default { $ex = $_ } } }
    is $ex.message, 'Type check failed for an element of @a; expected Int but got Str ("x")',
        "$meth reports the element-of wording";
}

# A Slip-flattened push type-checks each element
{
    my Int @a;
    my $ex;
    { @a.push(|(1, "x")); CATCH { default { $ex = $_ } } }
    is $ex.message, 'Type check failed for an element of @a; expected Int but got Str ("x")',
        'Slip element push type-checks each element';
}
