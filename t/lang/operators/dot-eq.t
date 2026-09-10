use Test;
plan 5;

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

my %a;
%a<foo> = <a b c b>;
%a<foo>.=unique;
is-deeply %a<foo>.List, <a b c>, ".= unique works on indexed hash values";
