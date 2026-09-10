use v6;
use Test;

# An `enum` declaration used in expression position (on the RHS of an
# assignment) must NOT swallow the terminating `;`. Previously the enum body
# parser consumed the trailing `;`, letting the expression parser absorb the
# next statement as an infix continuation (`my $e = enum <a b c>; say 42`
# mis-parsed `say` as an infix operator).

plan 8;

# Anonymous enum on an assignment RHS, followed by an ordinary statement.
{
    my $e = enum <a b c>;
    say 42;   # if absorbed, this line would be swallowed / mis-evaluated
    ok True, 'anon enum RHS does not absorb the following statement';
}

# The enum values are still usable after the assignment.
{
    my $e = enum <x y z>;
    is x.value, 0, 'anon enum first value';
    is z.value, 2, 'anon enum third value';
}

# Named enum on an assignment RHS.
{
    my $e = enum Fruit <Apple Banana Cherry>;
    is Fruit.^name, 'Fruit', 'named enum type name after RHS assignment';
    is Banana.value, 1, 'named enum value after RHS assignment';
}

# Plain statement context still works (control): terminator handled by the
# statement layer.
{
    enum Weekday <Mon Tue Wed>;
    is Tue.value, 1, 'statement-context enum still works';
}

# Multiple statements after an enum assignment all execute.
{
    my $captured = 0;
    my $e = enum <p q>;
    $captured = $captured + 1;
    $captured = $captured + 1;
    is $captured, 2, 'statements after enum assignment all run';
}

# Method chain / bare value use right after the assignment separator.
{
    my $e = enum <one two>;
    is (one, two).join(','), 'one,two', 'bare enum values usable after separator';
}
