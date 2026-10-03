use Test;

plan 6;

# The smiley of a typed `@`/`%` attribute is part of its container type.
class R { has Str:D @.errors; has Int @.n }
my Str @e = "a";
is R.new(errors => @e).errors.^name, 'Array[Str:D]', 'assigned from an Array[Str]';
is R.new.errors.^name, 'Array[Str:D]', 'the default empty container';
is R.new(n => (1, 2)).n.^name, 'Array[Int]', 'no smiley stays plain';
is-deeply R.new(errors => @e).errors, Array[Str:D].new("a"), 'is-deeply against Array[Str:D]';

class P { has Str:D %.h }
is P.new(h => {a => "b"}).h.^name, 'Hash[Str:D]', 'a hash attribute';
throws-like { R.new(errors => ("a", Str)) }, X::TypeCheck, 'elements are checked against Str:D';
