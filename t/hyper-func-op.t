use Test;

plan 11;

# Basic hyper func op with operator reference
{
    my @a = 1, 2, 3;
    my @b = 4, 5, 6;
    my &op = &[+];
    is-deeply @a >>[&op]<< @b, [5, 7, 9], '@a >>[&op]<< @b with &[+]';
    is-deeply @a <<[&op]<< @b, [5, 7, 9], '@a <<[&op]<< @b with &[+]';
    is-deeply @a <<[&op]>> @b, [5, 7, 9], '@a <<[&op]>> @b with &[+]';
    is-deeply @a >>[&op]>> @b, [5, 7, 9], '@a >>[&op]>> @b with &[+]';
}

# Scalar hyper func op returns scalar, not array
{
    my $a = 1;
    my $b = 6;
    my &op = &[~];
    is $a >>[&op]<< $b, "16", '$a >>[&op]<< $b returns scalar for scalars';
    is ($a >>[&op]<< $b).WHAT.gist, '(Str)', 'scalar hyper func op returns scalar type';
}

# DWIM behavior (extending shorter list)
{
    my @a = 1, 2, 3;
    my &op = &[+];
    is-deeply @a >>[&op]>> 10, [11, 12, 13], '@a >>[&op]>> 10 extends scalar';
    is-deeply 10 <<[&op]<< @a, [11, 12, 13], '10 <<[&op]<< @a extends scalar';
}

# Concatenation operator via func ref
{
    my @a = "a", "b", "c";
    my @b = "x", "y", "z";
    my &op = &[~];
    is-deeply @a >>[&op]<< @b, ["ax", "by", "cz"], '@a >>[&op]<< @b with ~';
}

# Hyper func op with regular hyper scalar result
{
    my $x = 3;
    my $y = 4;
    is $x >>+<< $y, 7, 'scalar >>+<< scalar returns scalar';
    is ($x >>+<< $y).WHAT.gist, '(Int)', 'scalar hyper returns Int not Array';
}
