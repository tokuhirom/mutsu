use v6;
use Test;

unit module EcosystemRoleBindingRegression;

plan 3;

# Net::Postgres 0.0.4 uses this shape while building Protocol::Postgres's
# packet schemas: a private role is a multi candidate's nominal constraint,
# and a private class's type object is the argument.
my role Serializable {
    method encode-to() { }
}

my proto map-type(|) { * }
multi map-type(Serializable $type) { 'serializable' }

my class SerializableInt does Serializable { }
multi map-type(Int:U) { 'int' }
multi map-type(Str:U) { 'str' }

my class Schema {
    method new(*@raw-elements) {
        @raw-elements.map({ map-type($_) })
    }
}

class FieldDescription {
    my $schema = Schema.new(Str, SerializableInt);
    method values() { $schema.join(',') }
}

is FieldDescription.values, 'str,serializable',
    'a private role constraint matches a private type object in a multi';

enum Format <Text Binary>;
role Encoder {
    multi method encode(Text, Any:D $value) { 'text' }
}
class EncoderImpl does Encoder { }

pass 'a role method with a bare enum member as a literal parameter compiles';

# A role in a nested package is resolved relative to its unit module when a
# type object is bound to an ordinary sub parameter.
package Packet {
    role Base { }
    class Child does Base { }
}

sub bind-packet(Packet::Base $class) { $class.^name }

is bind-packet(Packet::Child), 'EcosystemRoleBindingRegression::Packet::Child',
    'a nested package role accepts its composed type object';
