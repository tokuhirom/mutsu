use Test;

plan 5;

# `GLOBAL` is the implicit root namespace, so `package GLOBAL::X::Y { ... }`
# declares `X::Y` absolutely -- a class inside it is named `X::Y::Class`, not
# `GLOBAL::X::Y::Class`. This is how DBIish declares its exception types
# (`package GLOBAL::X::DBIish { class DriverNotFound is Exception {...} }`
# *inside* `unit class DBIish`), then refers to them as `X::DBIish::DriverNotFound`.

package GLOBAL::X::Demo {
    class Oops is Exception {
        has $.detail;
        method message { "oops: $.detail" }
    }
}

my $e = X::Demo::Oops.new(:detail('boom'));
is $e.detail, 'boom', 'class in GLOBAL:: package is reachable by its absolute name';
is $e.message, 'oops: boom', 'its methods work';
ok $e ~~ Exception, 'it is an Exception';

# The fully-qualified GLOBAL:: name still resolves to the same type.
ok GLOBAL::X::Demo::Oops === X::Demo::Oops,
    'GLOBAL::-prefixed and bare absolute names are the same type';

# A GLOBAL:: package nested inside another package still resolves absolutely
# (not relative to the enclosing package).
class Outer {
    package GLOBAL::X::Nested {
        class Inner { method who { 'inner' } }
    }
}
is X::Nested::Inner.new.who, 'inner',
    'GLOBAL:: package inside a class is absolute, not Outer::X::Nested';
