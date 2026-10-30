use Test;

plan 6;

# `$type.^isa($other)` on a type whose metaclass is a user subclass of
# Metamodel::ClassHOW (what Red's `model` is) asks about the TYPE, not about the
# HOW instance. rakudo's `^isa` returns 1/0.
my class MyHOW is Metamodel::ClassHOW { }

my \p = MyHOW.new_type(:name<P>);
p.^compose;
my \c = MyHOW.new_type(:name<C>);
c.^add_parent(p);
c.^compose;

ok c.^isa(p), 'a subclass isa its parent';
ok c.^isa(Any), 'isa Any';
ok c.^isa(c), 'isa itself';
ok p.^isa(p), 'a root isa itself';
nok p.^isa(c), 'a parent is not its child';
nok c.^isa(Int), 'unrelated type';
