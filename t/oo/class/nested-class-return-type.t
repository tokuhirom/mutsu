use Test;

plan 2;

# A method's return constraint resolves bare nested types in the declaring
# class's scope.  `Attribute` deliberately collides with the core type name;
# JSON::Infer uses this pattern in `JSON::Infer::Class`.
class Outer {
    class Attribute { }

    method make(--> Attribute) {
        Attribute.new
    }
}

my $value = Outer.new.make;
isa-ok $value, Outer::Attribute, 'return value has the nested class type';
is $value.^name, 'Outer::Attribute', 'nested return type is the declaring class type';
