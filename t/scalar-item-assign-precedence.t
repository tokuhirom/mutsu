use Test;

plan 13;

# Item assignment (`=`) to a bare scalar variable binds TIGHTER than the comma
# operator: `$x = 1, 2` parses as `($x = 1), 2`, so only the first element is
# assigned and the rest form a (sink-context) list.
{
    my $x;
    $x = 1, 99;
    is $x, 1, 'scalar item assignment takes only the first comma element';
}

# The parenthesized form is a single grouped operand and is assigned whole.
{
    my $x;
    $x = (1, 2, 3);
    is $x.elems, 3, 'parenthesized list RHS is assigned whole to a scalar';
}

# List assignment to an @-container absorbs the whole comma list.
{
    my @a;
    @a = 1, 2, 3;
    is @a.elems, 3, 'array list assignment absorbs the whole comma list';
}

# List assignment to a %-container absorbs the whole comma list.
{
    my %h;
    %h = a => 1, b => 2;
    is %h.elems, 2, 'hash list assignment absorbs the whole comma list';
}

# A hash/array element subscript is list assignment (leading sigil %/@).
{
    my %h;
    %h<k> = 1, 2;
    is %h<k>.elems, 2, 'hash element subscript assignment is list assignment';
}

# Chained scalar assignment still works.
{
    my ($x, $y);
    $x = $y = 5;
    is $x, 5, 'chained scalar assignment assigns the left';
    is $y, 5, 'chained scalar assignment assigns the right';
}

# Statement modifiers still attach after a bare scalar assignment.
{
    my $x = 0;
    $x = 7 if True;
    is $x, 7, 'statement modifier applies to a bare scalar assignment';
}

# A function call result followed by a comma assigns only the call result.
{
    sub gimme { 42 }
    my $x;
    $x = gimme(), 99;
    is $x, 42, 'scalar item assignment from a call takes only the first element';
}

# Compound RHS expressions bind as a single operand.
{
    my $x;
    $x = 1 + 2, 99;
    is $x, 3, 'scalar item assignment evaluates the first operand expression';
}

# Embedded scalar assignment (inside an expression / argument list) also binds
# item assignment tighter than comma.
{
    my ($x, $y);
    $x = $y = 1, 2;
    is $x, 1, 'nested scalar assignment assigns the outer the first element';
    is $y, 1, 'nested scalar assignment assigns the inner the first element';
}

# A scalar item assignment inside a call's argument list contributes one
# argument; the trailing comma starts the next argument.
{
    sub two-args($a, $b) { "$a|$b" }
    my $x;
    is two-args($x = 1, 2), "1|2",
        'embedded scalar assignment in an argument list yields two arguments';
}
