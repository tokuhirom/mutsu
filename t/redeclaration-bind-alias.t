use Test;

plan 1;

{
    my $x = 2;
    my $y := $x;
    my $x = 3;
    is $y, 3, 'redeclaring a lexical updates bound aliases in the same scope';
}
