use lib 't/lib';
use Test;
use SubstConstUser;

# From the Text::Utils distribution: a module sub interpolating a constant it
# imported (or declared with `our`) into an s/// pattern.
plan 3;

is collapse("1   2  3"), "1 2 3", 'imported constant with quantifier in s:g///';
is imported-once("a b"), "aXb", 'imported constant in s///';
is own-const("a-b-c"), "a+b+c", "module's own our constant in s:g///";
