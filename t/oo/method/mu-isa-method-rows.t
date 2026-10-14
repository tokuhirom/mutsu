use Test;

# `Mu.isa` is a row of the method table (ADR-11276 §9.104), which every
# receiver with a shape, and every receiver with none, reaches.

plan 17;

ok 42.isa(Int), 'an Int isa Int';
ok 42.isa(Cool), 'an Int isa Cool';
ok 42.isa(Mu), 'an Int isa Mu';
nok 42.isa(Str), 'an Int is no Str';
ok 42.isa("Int"), 'a type name';
ok 42.isa(3), 'a value stands for its type';
ok 3.5.isa(Cool), 'a Rat isa Cool';
nok 3.5.isa(Real), 'a role is not a class';
ok "s".isa(Str), 'a Str';
ok True.isa(Int), 'a Bool isa Int';
ok (1, 2).isa(List), 'a List';
ok [1, 2].isa(List), 'an Array isa List';
ok (1..2).isa(Cool), 'a Range isa Cool';
ok (1, 2).Seq.isa(Seq), 'a Seq';
ok (sub {}).isa(Code), 'a code object';
my Hash @a;
ok @a.isa(Array[Hash]), 'a typed Array isa its parameterized type';
class C {}
ok C.new.isa(C), 'an instance reaches the interpreter';
