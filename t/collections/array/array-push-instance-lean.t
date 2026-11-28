use Test;

# `@a.push($obj)` of a class instance takes the lean local push
# (`vm_data_push_lean`); it must store the instance itself, keep identity,
# and leave typed, aliased and closure-captured arrays to the general push.

plan 9;

class Animal { has $.name }
class Dog is Animal { }

my @animals;
for 1..100 -> $i {
    @animals.push(Animal.new(name => "Cat"));
    @animals.push(Dog.new(name => "Rex"));
}
is @animals.elems, 200, 'every instance is appended';
is @animals[0].^name, 'Animal', 'first element keeps its class';
is @animals[199].name, 'Rex', 'last element reads back';

my $d = Dog.new(name => 'Fido');
my @same;
@same.push($d) for ^3;
ok @same[0] === $d && @same[2] === $d, 'the instance itself is stored, not a copy';

my @alias := @animals;
@animals.push(Animal.new(name => 'Late'));
is @alias.elems, 201, 'a bound alias sees the push';

my Dog @dogs;
@dogs.push(Dog.new(name => 'ok'));
throws-like { @dogs.push(Animal.new(name => 'no')) }, X::TypeCheck,
    'a typed array still checks the element type';
is @dogs.elems, 1, 'the rejected push stored nothing';

my @captured;
my &adder = { @captured.push(Dog.new(name => 'c')) };
adder() for ^4;
is @captured.elems, 4, 'a closure-captured array is pushed';

my @failures;
@failures.push(Failure.new('boom'));
is @failures.elems, 1, 'a Failure instance is pushed without exploding';
