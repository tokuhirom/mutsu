use Test;

plan 6;

# `returns` / `of` accept a coercion type, the same way the `-->` arrow already did.
my sub arrow (Int $x --> Str()) { $x }
is-deeply arrow(42), '42', '--> Str() coerces the return value';

my sub trait-form (Int $x) returns Str() { $x }
is-deeply trait-form(42), '42', 'returns Str() coerces the return value';

my sub of-form (Int $x) of Str() { $x }
is-deeply of-form(42), '42', 'of Str() coerces the return value';

my sub sourced (Str $x) returns Int(Str) { $x }
is-deeply sourced('42'), 42, 'returns Int(Str) coerces from the named source type';

# A plain (non-coercion) type still works.
my sub plain (Int $x) returns Str { ~$x }
is-deeply plain(42), '42', 'returns Str is unchanged';

# A user-declared class is a complete then-expression in a ternary; it must not be parsed
# as a listop that gobbles the `!!`.
class Target {}
class Source { method make-one { self.DEFINITE ?? Target !! Target.new } }
isa-ok Source.make-one, Target, '?? UserClass !! ... parses in statement position';
