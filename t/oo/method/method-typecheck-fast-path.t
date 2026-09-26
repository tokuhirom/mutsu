use Test;

plan 2;

# Read-only methods use the compiled fast path. Its parameter diagnostics must
# still distinguish a nominal type mismatch from a :D concreteness mismatch.
class Lookup {
    method get(Str:D $key) { $key }
}

throws-like { Lookup.new.get(123) }, X::TypeCheck::Binding::Parameter,
    message => /'expected Str but got Int'/,
    'a fast-path method reports the base type in a mismatch';

throws-like { Lookup.new.get(Str) }, X::Parameter::InvalidConcreteness,
    message => /'must be an object instance'/,
    'a fast-path method reports a type object passed to a :D parameter';
