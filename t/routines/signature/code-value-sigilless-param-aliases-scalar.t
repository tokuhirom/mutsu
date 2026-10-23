use Test;

plan 7;

# A sigilless parameter of a code value (anonymous sub, pointy block, `my &b`)
# aliases a plain scalar variable argument, as a named sub's does (#14523).
{
    my $s = 1;
    my $c = sub (\t) { t = 6 };
    $c($s);
    is $s, 6, 'anonymous sub writes through \t to the caller scalar';
}

{
    my $s = 1;
    my $c = -> \t { t = 7 };
    $c($s);
    is $s, 7, 'pointy block writes through \t';
}

{
    my $s = 1;
    my &b = -> \t { t = 8 };
    b($s);
    is $s, 8, 'a code variable called by name writes through \t';
}

{
    my ($x, $y) = 1, 2;
    my $m = -> \a, \b { a = b; b = 0 };
    $m($x, $y);
    is "$x $y", "2 0", 'two sigilless parameters alias two scalars';
}

{
    my @a = 1, 2;
    my $e = -> \t { t = 9 };
    $e(@a[0]);
    is-deeply @a, [9, 2], 'an element argument still aliases';
}

{
    my $c = sub (\t) { t = 6 };
    throws-like { $c(5) }, X::Assignment::RO, 'a literal argument stays read-only';
    my $r = sub (\t) { t * 2 };
    my $s = 4;
    is $r($s) ~ $s, "84", 'reading through \t leaves the scalar alone';
}
