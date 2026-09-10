use Test;

# A user-defined subclass of a builtin type may override an inherited native
# method; the user override must win over the native handler. Regression for
# IO::Blob (`class IO::Blob is IO::Handle` overriding .get/.lines/.getc/...),
# which previously died with "Expected IO::Handle" because the native
# IO::Handle.get shadowed the user method.

plan 8;

class MyHandle is IO::Handle {
    has @.lines-data;
    has $.idx = 0;
    method get { @!lines-data[$!idx++] // Nil }
    method eof { $!idx >= @!lines-data.elems }
    method extra { "extra-method" }
}

my $h = MyHandle.new(lines-data => <a b c>);
is $h.get, 'a', 'overridden .get returns the user value (not native)';
is $h.get, 'b', 'overridden .get is stateful';
nok $h.eof, 'overridden .eof works while data remains';
$h.get;
ok $h.eof, 'overridden .eof reports end';
is $h.extra, 'extra-method', 'a user-only (non-native) method still dispatches';

# The override must still resolve through a deeper MRO.
class Base is IO::Handle { method get { "base" } }
class Derived is Base { }
is Derived.new.get, 'base', 'inherited user override (grandparent) wins over native';

# A subclass that does NOT override keeps using the inherited native method
# (here just confirm the class builds and a user method on it dispatches).
class Plain is IO::Handle { method tag { "plain" } }
is Plain.new.tag, 'plain', 'non-overriding subclass dispatches its own method';

# Overriding a native method on a non-IO builtin parent works too.
class MyStr is Str { method Str { "custom-str" } }
is MyStr.new.Str, 'custom-str', 'override of a native method on a Str subclass wins';
