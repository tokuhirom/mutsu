use Test;

# A constant bound to a package is a valid qualifier for the types and enum
# values declared in that package. Found via the Terminal::MultiProgress
# distribution, whose test does `constant Event = Terminal::MultiProgress::Event`
# and then compares against `Event::Status::Started`.

plan 7;

class Outer::Inner {
    enum Status <Started Finished>;
    class Nested { method hi { 'hi' } }
    our sub f { 'f' }
}

constant E = Outer::Inner;

ok E::Status::Started === Outer::Inner::Status::Started,
    'an enum value through the alias is the same value';
is E::Status::Finished.value, 1, 'the enum value keeps its value';
ok E::Status === Outer::Inner::Status, 'the enum type through the alias';
is E::Nested.^name, 'Outer::Inner::Nested', 'a nested class through the alias';
is E::Nested.hi, 'hi', 'a method call on the aliased class';
is E::f(), 'f', 'a qualified call through the alias still works';

my constant L = Outer::Inner;
ok L::Status::Started === Outer::Inner::Status::Started, 'a `my constant` alias';
