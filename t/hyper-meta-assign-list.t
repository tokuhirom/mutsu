use Test;

plan 10;

# Hyper meta-assignment over a literal list of scalar lvalues mutates each one.
{
    my $a = 'apple';
    my $b = 'blueberry';
    my $c = 'cherry';
    my @r = (($a, $b, $c) »~=» <pie tart>);
    is @r.join(','), 'applepie,blueberrytart,cherrypie', 'returns the new values';
    is $a, 'applepie', '$a mutated';
    is $b, 'blueberrytart', '$b mutated';
    is $c, 'cherrypie', '$c mutated (rhs cycled)';
}

{
    my $x = 1;
    my $y = 2;
    my $z = 3;
    ($x, $y, $z) »+=» (10, 20, 30);
    is $x, 11, '+= a';
    is $y, 22, '+= b';
    is $z, 33, '+= c';
}

{
    my $a = 2;
    my $b = 3;
    ($a, $b) »*=» 5;
    is $a, 10, '*= scalar broadcast a';
    is $b, 15, '*= scalar broadcast b';
}

{
    my @arr = 1, 2, 3;
    @arr »+=» 100;
    is @arr.join(','), '101,102,103', 'hyper += on array elements';
}
