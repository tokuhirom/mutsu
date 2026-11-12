use Test;

# A method's `--> T` return spec is resolved against its owner and checked
# on every call; #17331 remembers the resolution per (owner, spec).

plan 7;

class Shape { }

class Holder {
    method good(--> Shape) { Shape.new }
    method bad(--> Shape)  { 42 }
    method any(--> Mu)     { 42 }
    method maybe(--> Shape:D) { Shape }
}

for ^2 {
    isa-ok Holder.good, Shape, 'a matching return value passes';
    throws-like { Holder.bad }, X::TypeCheck::Return, 'a mismatching one dies';
}
is Holder.any, 42, '--> Mu admits anything';
throws-like { Holder.maybe }, X::TypeCheck::Return, 'a :D smiley is checked';

role Typed[::T] {
    method get($x --> T) { $x }
}
class TI does Typed[Int] { }
throws-like { TI.new.get("str") }, X::TypeCheck::Return,
    'a --> T capture is resolved per composition';
