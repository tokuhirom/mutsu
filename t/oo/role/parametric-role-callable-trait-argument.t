use v6;
use Test;

plan 1;

# Audio::Hydrogen (through XML::Class's SerialiseX[&serialiser]) exposed
# callable role parameters becoming Nil when the role is mixed into an
# Attribute by a custom trait.
role Callback[&code] {
    has &.code = &code;
    method run($value) { self.code.($value) }
}

multi sub trait_mod:<is>(Attribute $attr, :&callback!) {
    $attr does Callback[&callback];
}

class Example {
    sub adjust($value) { $value + 1 }
    has $.value is callback(&adjust);
}

is Example.^attributes[0].run(41), 42,
    'a callable role argument is available in the role attribute default';
