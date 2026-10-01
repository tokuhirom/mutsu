use Test;

# A block's `let`/`temp` save frame must see a save wherever it hides in the
# block's own scope -- an operand, a ternary arm, a list element, a
# `given`/`when` body (ADR-0137 port of `has_let_deep`). Without the frame
# nothing restores the save and the temporary value became permanent.

plan 7;

{
    my $a = 1;
    { 1 ?? (temp $a = 5) !! 0; is $a, 5, 'temp in a ternary arm is in effect inside the block' }
    is $a, 1, 'temp in a ternary arm is restored at block exit';
}

{
    my $a = 1;
    { given 1 { when 1 { temp $a = 5 } } }
    is $a, 1, 'temp in a when body inside a given is restored at the block exit';
}

{
    my $a = 1;
    { my $r = (temp $a = 7) + 1; is $r, 8, 'temp as an operand yields its value' }
    is $a, 1, 'temp as an operand is restored at block exit';
}

{
    my $b = 1;
    { for ^1 { }; my @l = 1, (temp $b = 3); is @l[1], 3, 'temp in a list element yields its value' }
    is $b, 1, 'temp in a list element is restored at block exit';
}
