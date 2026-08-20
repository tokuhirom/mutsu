use Test;

# ADR-0047 P1 regression pin: a `my`-scoped class registers under a mangled
# storage name (`Name\0<decl-id>`). throws-like's named attribute matchers
# (`got => TestSink`) compare the ACTUAL value's user-facing type name against
# the matcher type object's name, so the matcher side must be demangled too —
# otherwise a lexical class type object never matches its own attribute value
# (caught by Cro::Core's composer.rakutest / connection-conditional.rakutest
# in the battery gate).

plan 2;

my class TestSink { }
my class X::TestFoo is Exception {
    has $.got;
    method message() { "test foo" }
}

throws-like { die X::TestFoo.new(got => TestSink) }, X::TestFoo,
    got => TestSink,
    'attribute matcher accepts a lexical class type object';

my class Widget { }
throws-like { die X::TestFoo.new(got => Widget.new) }, X::TestFoo,
    got => Widget,
    'attribute matcher accepts an instance of a lexical class';
