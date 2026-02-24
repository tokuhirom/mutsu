use Test;
plan 8;

my $a = 0;
$a = 1 if True;
is $a, 1, "postfix if true";

my $b = 1;
$b = 0 unless False;
is $b, 0, "postfix unless false";

my $c = 0;
$c++ for 1..5;
is $c, 5, "postfix for";

my $d = 0;
$d++ while $d < 3;
is $d, 3, "postfix while";

my $e = 0;
$e++ until $e == 4;
is $e, 4, "postfix until";

{
    my $a = "oops";
    { $a = "ok" } unless 0;
    is $a, "ok", "postfix unless on bare block";
}

{
    my $a = "oops";
    { $a = $^x } unless 0;
    is $a, 0, "postfix unless on placeholder block";
}

is (1, 2, unless 0), "1 2", "unless terminates expression list after comma";
