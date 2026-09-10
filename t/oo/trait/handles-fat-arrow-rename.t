use v6;
use Test;

plan 4;

# NOTE: Rakudo only accepts a QUOTED target in a fat-arrow handles pair
# (`exposed => 'target'`); a bare identifier is parsed as a term. This pin
# uses quoted targets throughout.

# `handles (exposed => 'target')` fat-arrow rename delegation on an attribute
# (roast/integration/advent2009-day11.t). Previously only the colon-pair form
# (`handles (:exposed<target>)`) parsed.
class Dog { has $.name; }
class DogWalker {
    has $.name;
    has Dog $.dog handles (dog_name => 'name');
}
my $fido = Dog.new(name => 'Fido');
my $bob = DogWalker.new(name => 'Bob', dog => $fido);
is $bob.name, 'Bob', 'own accessor still works';
is $bob.dog_name, 'Fido', 'fat-arrow rename delegation (quoted target)';

# Multiple pairs in one handles list.
class Engine { has $.model; method start() { 'vroom' } }
class Car {
    has Engine $.engine handles (engine_model => 'model', ignite => 'start');
}
my $car = Car.new(engine => Engine.new(model => 'V8'));
is $car.engine_model, 'V8', 'first fat-arrow rename in the list';
is $car.ignite, 'vroom', 'second fat-arrow rename in the same list';
