use Test;

plan 13;

# Basic rindex
is rindex("Hello World", "l"), 9, "rindex finds last occurrence";
is rindex("Hello World", "H"), 0, "rindex at beginning";
is rindex("Hello World", "d"), 10, "rindex at end";
ok !defined(rindex("Hello World", "x")), "rindex returns Nil for no match";

# With position argument
is rindex("Hello World", "l", 9), 9, "rindex with pos at match";
is rindex("Hello World", "l", 8), 3, "rindex with pos before match";

# Empty needle
is rindex("Hello", ""), 5, "rindex empty needle returns length";
is rindex("", ""), 0, "rindex both empty";

# Method form
is "Hello World".rindex("l"), 9, ".rindex method";
is "Hello World".rindex("l", 8), 3, ".rindex method with pos";

# rindex on non-strings
ok 3459.rindex(5) == 2, "rindex on integers";

# List of needles
is "foobar".rindex(<o a>), 4, "rindex with list of needles";

# Negative position returns Failure
ok rindex("xxyxx", "y", -1) ~~ Failure, "rindex with negative pos returns Failure";
