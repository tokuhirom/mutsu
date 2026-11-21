use Test;

# #18326: in an interpolating string a variable name continues through `'`
# (as through `-`) when a letter or `_` follows, as in code: `"$don't"` reads
# `$don't`, and `"$src's"` names an undeclared `$src's`.

plan 7;

my $don't = 3;
my $src = 1;
my $x-y = 2;
is "x $don't y", 'x 3 y', 'an apostrophe inside the name is part of it';
is "'$src' is", "'1' is", 'a quote after the name ends it';
is "it's $src'", "it's 1'", 'a trailing apostrophe is literal text';
is "$x-y $src-", '2 1-', 'hyphens follow the same rule';
my @a's = 1, 2;
is "@a's[]", '1 2', 'an array name with an apostrophe';

throws-like q[my $src = 1; say "$src's"], X::Undeclared,
    'a name continued by an apostrophe is undeclared when nothing declares it';
is "{$src}'s", "1's", 'braces end the name before an apostrophe';
