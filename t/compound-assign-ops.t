use Test;

plan 17;

# or=
{
    my $x = 0; $x or= 5; is $x, 5, 'or= when falsy assigns RHS';
    my $y = 3; $y or= 5; is $y, 3, 'or= when truthy keeps LHS';
}

# and=
{
    my $x = 1; $x and= 5; is $x, 5, 'and= when truthy assigns RHS';
    my $y = 0; $y and= 5; is $y, 0, 'and= when falsy keeps LHS';
}

# orelse=
{
    my $x = 3; $x orelse= 5; is $x, 3, 'orelse= when defined keeps LHS';
    my $y = Int; $y orelse= 5; is $y, 5, 'orelse= when undefined assigns RHS';
}

# andthen=
{
    my $x = 3; $x andthen= 5; is $x, 5, 'andthen= when defined assigns RHS';
    my $y = Int; $y andthen= 5; is $y, '(Int)', 'andthen= when undefined keeps LHS';
}

# +<=  (bit shift left assign)
{
    my $x = 8; $x +<= 2; is $x, 32, '+<= shifts left';
}

# +>=  (bit shift right assign)
{
    my $x = 8; $x +>= 2; is $x, 2, '+>= shifts right';
}

# div=
{
    my $x = 10; $x div= 3; is $x, 3, 'div= integer divides';
}

# lcm=
{
    my $x = 12; $x lcm= 8; is $x, 24, 'lcm= computes LCM';
}

# gcd=
{
    my $x = 12; $x gcd= 8; is $x, 4, 'gcd= computes GCD';
}

# min=
{
    my $x = 10; $x min= 5; is $x, 5, 'min= assigns minimum';
    my $y = 3; $y min= 5; is $y, 3, 'min= keeps LHS when smaller';
}

# max=
{
    my $x = 3; $x max= 5; is $x, 5, 'max= assigns maximum';
    my $y = 10; $y max= 5; is $y, 10, 'max= keeps LHS when larger';
}
