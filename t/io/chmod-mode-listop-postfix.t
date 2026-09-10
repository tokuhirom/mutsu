use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 4;

my $file1 = make-temp-file(:content("a"));
my $file2 = make-temp-file(:content("b"));

my @first = chmod 0o700, $file1;
is +@first, 1, "chmod with explicit mode updates one file";

my @result = chmod $file1.IO.mode, $file2;
is +@result, 1, "chmod accepts mode value from method-call expression";
is-deeply @result[0], $file2, "chmod returns changed file name";
is $file2.IO.mode, "0700", "chmod copied mode from the first file";
