use Test;
use lib 't/lib';
use DoesRoleInitUnitClass;

# `$sub does R(v)` inside a sub of a `unit class` module, where R is a
# `my role` of that unit and the sub runs from another file (found via
# Getopt::Long's `is getopt` trait): R must resolve as the role, so `v` is its
# initializer, not an argument of a coercion call.
plan 2;

sub foo(:$bar) is getopt { }
is &foo.getopt, 'plain', 'does R(v) in a unit-class sub initializes the lexical role';
ok &foo ~~ Callable, 'the sub is still callable';
