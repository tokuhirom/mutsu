use Test;

plan 6;

# Rightmost named argument should win when the same key is provided multiple times.
{
    sub np(:$assoc) { $assoc }
    sub snp(*%n) { %n<assoc> }

    is np(|{:assoc<list>}, :assoc<left>), 'left', 'rightmost named argument wins (1)';
    is np(:assoc<left>, |{:assoc<list>}), 'list', 'rightmost named argument wins (2)';
    is snp(|{:assoc<list>}, :assoc<left>), 'left', 'rightmost named argument wins (3)';
    is snp(:assoc<left>, |{:assoc<list>}), 'list', 'rightmost named argument wins (4)';
}

# Simple duplicate named args
{
    sub f(:$x) { $x }
    is f(:x(1), :x(2)), 2, 'rightmost named wins for simple duplicate named args';
    is f(:x(2), :x(1)), 1, 'rightmost named wins for simple duplicate named args (reversed)';
}
