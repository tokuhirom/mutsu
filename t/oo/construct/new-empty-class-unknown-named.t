use Test;

# A class the program declares with no attribute has a known, empty
# attribute set: the default constructor ignores a named argument that names
# no attribute, as it does for a class with attributes, and the
# constructor lane serves it.

plan 6;

class Empty { }
class WithMethod { method hi { 'hi' } }
class Parent { has $.p = 1 }
class Child is Parent { }

ok Empty.new(foo => 1) eqv Empty.new, 'an unknown named argument is not stored';
is WithMethod.new(x => 2).hi, 'hi', 'a class with only methods constructs';
is Child.new(p => 5).p, 5, 'an attribute-less subclass still builds its parent attribute';
ok Child.new(q => 3) eqv Child.new, '... and ignores an unknown one';
is (^50).map({ Empty.new }).grep(Empty).elems, 50, 'repeated construction';

class X::Mine is Exception { }
lives-ok { X::Mine.new(message => 'x') }, 'an Exception subclass keeps its attribute bag';

# vim: expandtab shiftwidth=4
