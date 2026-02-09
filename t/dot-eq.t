use Test;
plan 4;

my $x = "hello";
$x .= uc;
is $x, "HELLO", ".= uc";

my $y = "WORLD";
$y .= lc;
is $y, "world", ".= lc";

my $z = "abc";
$z .= flip;
is $z, "cba", ".= flip";

my $w = "  hi  ";
$w .= trim;
is $w, "hi", ".= trim";
