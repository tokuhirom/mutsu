use v6;
use Test;

# `"&name(ARGS)"` interpolates a call, and like an interpolated variable it
# takes a following postfix chain. Identity::Utils builds a distribution name
# with `"&short-name($identity).subst('::','-',:g)-..."`, which mutsu left as
# literal `.subst(...)` text.

plan 7;

sub f($x) { "a::b$x" }
sub g($a, $b) { "$a|$b" }

is "&f(1).subst('::', '-', :g)-x", 'a-b1-x', 'method call chained on an interpolated call';
is "&f(1).uc()!", 'A::B1!', 'argument-less method call with parens';
is "&f(1).uc-x", 'a::b1.uc-x', 'a method name without parens stays literal';
is "&g('x,y', 2)", 'x,y|2', 'a comma inside a quoted argument does not split it';
is "&g(f(1), 2)", 'a::b1|2', 'a nested call is one argument';
is "mail&f(3).", 'maila::b3.', 'a trailing dot is literal text';
is "x&y", 'x&y', 'an & without a call is literal text';
