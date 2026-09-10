use v6;
use Test;

# A postfix `.=` mutator yields its variable as an lvalue, so a prefix `++`/`--`
# can operate on it: `++$a.=abs` is `++($a.=abs)`.
# Regression: mutsu rejected this with "prefix:<++> requires mutable arguments".

plan 8;

{
    my $a = -5;
    is (++$a.=abs), 6, '++$a.=abs returns 6';
    is $a, 6, '  ... and $a is now 6';
}

{
    my $b = 3.5;
    is (--$b.=floor), 2, '--$b.=floor returns 2';
    is $b, 2, '  ... and $b is now 2';
}

{
    my $c = "5";
    is (++$c.=Int), 6, '++$c.=Int coerces then increments';
    is $c, 6, '  ... and $c is now 6';
}

{
    # Plain `.=` chain still increments the same variable.
    my $d = -10;
    ++$d.=abs;
    is $d, 11, 'statement form mutates the variable';
}

{
    my $e = -1;
    is (--$e.=abs), 0, '--$e.=abs returns 0';
}
