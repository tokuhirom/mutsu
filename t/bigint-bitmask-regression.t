use Test;

plan 4;

my $mask64 = (1 +< 64) - 1;
is $mask64.fmt("%064b"), "1" x 64, "(1 +< 64) - 1 keeps integer precision";
is (-1 +& $mask64).fmt("%064b"), "1" x 64, "bitwise and with 64-bit mask stays precise";

my $mask65 = (1 +< 65) - 1;
is $mask65.fmt("%065b"), "1" x 65, "(1 +< 65) - 1 keeps integer precision";
is (-1 +& $mask65).fmt("%065b"), "1" x 65, "bitwise and with 65-bit mask stays precise";
