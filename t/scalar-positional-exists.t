use Test;

plan 21;

# A non-Positional value is a one-element list holding itself under a
# positional subscript, so index 0 is the only one that exists.
{
    my $i = 5;
    ok $i[0]:exists, 'index 0 of an Int exists';
    nok $i[1]:exists, 'index 1 of an Int does not exist';
    nok ($i[0]:!exists), ':!exists negates on a scalar';
    is-deeply ($i[0,1]:exists).List, (True, False), 'a slice reports per index';
    is-deeply ($i[*]:exists).List, (True,), 'a zen slice of a scalar has one element';
    ok $i[*-1]:exists, 'a WhateverCode index resolves against the one element';
    is $i.EXISTS-POS(0), True, 'EXISTS-POS(0) on an Int';
    is $i.EXISTS-POS(1), False, 'EXISTS-POS(1) on an Int';
}

{
    my $s = "ab";
    ok $s[0]:exists, 'index 0 of a Str exists (a Str is not Positional)';
    nok $s[1]:exists, 'a Str is one element, not one per character';
}

{
    my $r = <1/2>;
    ok $r[0]:exists, 'index 0 of a Rat exists';
    nok $r[1]:exists, 'index 1 of a Rat does not exist';
}

{
    my $c = sub { 1 };
    ok $c[0]:exists, 'index 0 of a Sub exists';
    my $p = (a => 1);
    ok $p[0]:exists, 'index 0 of a Pair exists';
    nok $p[1]:exists, 'index 1 of a Pair does not exist';
}

# An undefined value is the *empty* list: nothing exists.
{
    my $n;
    nok $n[0]:exists, 'index 0 of an undefined scalar does not exist';
    my $u = Int;
    nok $u[0]:exists, 'index 0 of a type object does not exist';
}

# An associative subscript on a scalar finds no key.
{
    my $i = 5;
    nok ($i<a>:exists), 'an associative subscript on a scalar does not exist';
}

# Real containers keep their own answers.
{
    my @a = 1, 2, 3;
    ok @a[*-1]:exists, 'a WhateverCode index on an array resolves to its length';
    nok @a[*-9]:exists, 'a WhateverCode index below the start does not exist';
    my @e;
    nok @e[0]:exists, 'an empty array has no index 0';
}
