use Test;
plan 8;

ok !(one(1..2)), 'one(range) flattens iterable arguments';

{
    my $c = 0;
    for (-4..4)X(-4..4) -> ($x, $y) {
        if $x & $y == -1 | 0 | 1 {
            $c++;
        }
    }
    is $c, 9, 'for pointy param list with parens binds tuple values';
}

{
    my $matched = False;
    {
        my $c = 0;
        for (-4..4)X(-4..4) -> ($x, $y) {
            if $x & $y == -1 | 0 | 1 {
                $c++;
            }
        }
    }
    given 1 {
        when 0 | 1 | 2 {
            $matched = True;
        }
    }
    ok $matched, 'given statement after block is not parsed as postfix modifier';
}

ok do if 1 ne 2|3|4 { 1 } else { 0 }, 'ne with junction is true when all are different';
ok do if 1 ne 1|3|4 { 0 } else { 1 }, 'ne with junction is false when any matches';

{
    my $foo = 0;
    sub infix:<|>(*@a) { $foo++; any(|@a) };
    sub infix:<&>(*@a) { $foo++; all(|@a) };
    ok do if 1 | 2 | 3 | 4 == 3 { 1 } else { 0 }, 'symbolic infix shadowing works';
    ok do if 1 & 2 & 3 & 4 == 3 { 0 } else { 1 }, 'symbolic infix shadowing works for &';
    is $foo, 2, 'shadowed infix operators preserve lexical side effects';
}
