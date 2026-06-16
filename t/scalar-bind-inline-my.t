use Test;

# Binding a scalar to an inline-declared scalar (`$y := my $x`,
# `my $a := my $b`) must alias the same container, so writes through either
# name are visible on the other -- matching Rakudo. Previously the inline `my`
# declaration's name was lost, so the bind snapshotted Nil and shared no cell.

plan 11;

# Rebind an existing scalar to an inline my
{
    my $y = 5;
    $y := my $x;
    $x = 9;
    is $y, 9, 'write through $x reaches $y';
    $y = 7;
    is $x, 7, 'write through $y reaches $x (bidirectional)';
}

# The inline-declared variable starts undefined and is a normal lexical
{
    my $y = 5;
    $y := my $x;
    ok $x.defined.not, 'inline $x starts undefined';
    ok $y.defined.not, 'rebound $y is also undefined';
}

# Declaration-and-bind to an inline my (`my $a := my $b`)
{
    my $a := my $b;
    $b = 5;
    is $a, 5, 'write through $b reaches $a';
    $a = 9;
    is $b, 9, 'write through $a reaches $b';
}

# Independent decl+inline binds do not interfere
{
    my $a := my $b;
    my $c := my $d;
    $b = 1;
    $d = 2;
    is $a, 1, 'first pair independent';
    is $c, 2, 'second pair independent';
}

# Three-way aliasing chain
{
    my $y;
    $y := my $x;
    my $z := $x;
    $x = 5;
    is $y, 5, 'chain reaches $y';
    is $z, 5, 'chain reaches $z';
}

# Regression: pre-declared scalar bind still aliases
{
    my $b;
    my $a := $b;
    $b = 1;
    is $a, 1, 'pre-declared scalar bind still shares a cell';
}
