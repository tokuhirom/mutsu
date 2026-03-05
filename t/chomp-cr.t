use Test;

plan 9;

# chomp should remove \n, \r\n, and \r, but only one at a time

is "foo\n".chomp, "foo", "chomp removes trailing \\n";
is "foo\r".chomp, "foo", "chomp removes trailing \\r";
is "foo\r\n".chomp, "foo", "chomp removes trailing \\r\\n";

# Only one newline removed at a time
is "foo\n\n".chomp, "foo\n", "chomp removes only one trailing \\n";
is "foo\r\r".chomp, "foo\r", "chomp removes only one trailing \\r";
is "foo\r\n\r\n".chomp, "foo\r\n", "chomp removes only one trailing \\r\\n";

# chomp function (non-destructive)
my $foo = "bar\n";
chomp($foo);
is $foo, "bar\n", "chomp() function does not modify the variable";
is chomp($foo), "bar", "chomp() function returns chomped string";

is "".chomp, "", "chomp on empty string is empty";
