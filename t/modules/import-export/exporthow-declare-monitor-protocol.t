use lib 't/lib';
use Test;

# The full HOW registration protocol for an EXPORTHOW::DECLARE'd class,
# in the exact shape OO::Monitors uses (see DeclareMonitorish.rakumod):
# new_type/callsame, add_attribute via the HOW, add_method wrap + the
# fully-qualified native re-add, compose's method_table probe + anon-method
# BUILDALL/POPULATE install, and the constructor running the user BUILDALL
# so the HOW-added attribute is initialized before any method runs.

use DeclareMonitorish;

plan 5;

traced Greeter {
    has $.name;
    method hi() { "hi, " ~ $.name }
    method motto() { "salve" }
}

my $g = Greeter.new(name => 'ann');
my $attr = Greeter.^attributes.first(*.name eq '$!TRACE-log');
ok $attr.defined, 'the HOW-added attribute exists on the class (^attributes)';
is $attr.get_value($g), 'init;',
    'the compose-installed POPULATE ran at construction and seeded the attribute';

is $g.hi, 'hi, ann', 'a wrapped method still returns its original result';
is $attr.get_value($g), 'init;hi;/hi;',
    'the add_method wrapper ran around the call (enter + LEAVE both fired)';

# Type-object calls take the non-DEFINITE path of the wrapper.
is Greeter.motto, 'salve', 'a type-object method call passes through the wrapper';
