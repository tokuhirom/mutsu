use Test;

plan 5;

# A Capture built from `\($a)` / `\(:$a)` captures the variable's *container*, so
# mutating the Capture element (via `++`/`--`) writes through to the original
# variable. Mirrors roast/S02-types/capture.t subtests 28-29.

# Positional element.
{
    my $a = 41;
    my $c = \($a);
    $c[0]++;
    is $a, 42, 'incrementing a Capture positional writes through to the variable';
}

# Named element.
{
    my $a = 41;
    my $c = \(:$a);
    $c<a>++;
    is $a, 42, 'incrementing a Capture named element writes through';
}

# Two variables in one capture stay independent.
{
    my $x = 10;
    my $y = 20;
    my $c = \($x, $y);
    $c[0]++;
    $c[1]--;
    is $x, 11, 'first capture element writes through independently';
    is $y, 19, 'second capture element writes through independently';
}

# A non-variable element captures by value (no container to alias).
{
    my $c = \(5);
    is $c[0], 5, 'a literal capture element is just a value';
}
