use Test;

# A double-quoted regex term is a qq string: `%h<k>` and `&f()` interpolate,
# while a bare `%` or `&name` is literal text.

plan 4;

my %h = x => 1;
sub f { 'F' }

ok "a 1 b" ~~ /"a %h<x> b"/, 'a hash element interpolates';
ok "a F b" ~~ /"a &f() b"/, 'a routine call interpolates';
ok "50%" ~~ /"50%"/, 'a lone % is literal';
ok "x &f y" ~~ /"x &f y"/, 'a &name without a call is literal';
