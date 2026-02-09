use Test;
plan 4;

my $out = "";
say "hello", " ", "world";
is 1, 1, "say with multiple args compiles";

say 1, 2, 3;
is 1, 1, "say with numbers compiles";

print "a", "b";
print "c\n";
is 1, 1, "print with multiple args compiles";

put "x", "y";
is 1, 1, "put with multiple args compiles";
