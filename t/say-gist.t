use Test;
plan 2;

my @a = 1, 2;
is @a.join(" ") ~ "\n", "1 2\n", 'say joins args with space';
is (1/3).gist, "<1/3>", 'gist for Rat';
