use Test;

# From the Net::Ethereum distribution: `unit class Net::Ethereum` declares a
# nested `class X is Exception` and then `class X::JSONAPI is X`, and its code
# (and test) write `Net::Ethereum::X::JSONAPI::InvalidResponse.new`. The head
# `X` of the compound name must resolve to the enclosing package's own class,
# not to the core `X::` exception namespace.
plan 8;

class Outer {
    class X is Exception {
        has $.payload is rw;
        method message { "payload: $!payload" }
    }
    class X::Api is X { }
    class X::Api::Bad is X::Api { }

    method bad() { X::Api::Bad.new(:payload('inside')) }
}

is Outer::X.^name, 'Outer::X', 'the nested class keeps its qualified name';
is Outer::X::Api.^name, 'Outer::X::Api', 'a compound child is reachable by its qualified name';
is Outer::X::Api::Bad.^name, 'Outer::X::Api::Bad', 'and so is a deeper one';
is Outer.bad.message, 'payload: inside', 'inside the package the short compound name finds it';
is Outer::X::Api::Bad.new(:payload('outside')).message, 'payload: outside', 'a qualified construction works';

my $caught;
try { die Outer::X::Api::Bad.new(:payload('thrown')); CATCH { default { $caught = $_ } } }
isa-ok $caught, Outer::X::Api, 'the thrown exception is the nested class';
is $caught.message, 'payload: thrown', 'with its own message';
ok Outer::X::Api::Bad ~~ Exception, 'the nested hierarchy still reaches Exception';
