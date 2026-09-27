use Test;

plan 5;

# `.name` on a TYPE OBJECT whose class declares its own public `$.name` attribute
# resolves to that accessor; reading an instance attribute off a type object is an
# error. An *instance* still reads the attribute normally.

class Dog { has $.name }

is Dog.new(name => 'Fido').name, 'Fido', '.name on an instance reads the attribute';
dies-ok { Dog.name }, '.name on the type object dies (attribute lookup on type object)';
throws-like 'class D { has $.name }; D.name', Exception,
    '.name on a type object throws an Exception';

# A class WITHOUT a `name` attribute has no `name` method at all: `.^name`
# is the type-name introspection (#9776).
class Plain { }
throws-like { Plain.name }, X::Method::NotFound,
    '.name on a class without a name attribute is X::Method::NotFound';

# Routine .name is unaffected.
sub greet() { }
is &greet.name, 'greet', '&sub.name still returns the routine name';
