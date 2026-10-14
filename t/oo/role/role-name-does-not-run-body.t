use Test;
plan 5;

my @log;
my role R { @log.push: "body"; method m { 42 } }

is R.^name, "R", "R.^name";
is R.WHAT.raku, "R", "R.WHAT.raku";
is-deeply @log, [], "naming the role does not run its body";
is R.m, 42, "calling a role method puns the role";
is @log.elems, 1, "the pun ran the body once";
