use Test;

# XML::Class: `Attribute.type` of `has Str %.h` is the type object
# `Associative[Str]`, which must bind to `Cool %o` / `Cool @o` by element type.
plan 7;

multi sub f(Cool %o) { "cool-hash" }
multi sub f(Mu $o) { "mu" }
multi sub g(Cool @o) { "cool-array" }
multi sub g(Mu $o) { "mu" }

class C { has Str %.h; has Int @.a; }
my ($ha, $aa) = C.^attributes;

is f($ha.type), "cool-hash", "Associative[Str] type object binds to Cool %o";
is g($aa.type), "cool-array", "Positional[Int] type object binds to Cool @o";
is f(Associative[Int]), "cool-hash", "Associative[Int] binds to Cool %o";
is f(Associative), "mu", "bare Associative does not";
is g(Positional[Str]), "cool-array", "Positional[Str] binds to Cool @o";
is f(my Str %h), "cool-hash", "typed hash still binds";
is g(my Int @arr), "cool-array", "typed array still binds";
