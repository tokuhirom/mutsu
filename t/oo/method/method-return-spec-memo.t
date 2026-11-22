use Test;

# A method's `--> T` return spec is resolved against its owner and checked
# on every call; #17331 remembers the resolution per (owner, spec).

plan 11;

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

# A definite return (`--> Nil`, `--> True`, `--> Empty`, `--> pi`) names the
# value the call returns, whatever the body's last statement answers.
class Definite {
    has $.n = 0;
    method nil(--> Nil)  { $!n++ }
    method t(--> True)   { $!n++ }
    method e(--> Empty)  { $!n }
    method p(--> pi)     { $!n }
}
my $def = Definite.new;
is ($def.nil, $def.nil).raku, '(Nil, Nil)', '--> Nil returns Nil on every call';
is ($def.t, $def.t).raku, '(Bool::True, Bool::True)', '--> True returns True';
is ($def.e.raku, $def.p).join(' '), 'Empty 3.141592653589793', '--> Empty and --> pi';
is $def.n, 4, 'the bodies still ran';
