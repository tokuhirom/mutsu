use lib 't/lib';
use Test;

# EXPORTHOW::DECLARE — a `use`d module can register a NEW class-like
# declarator keyword:
#   my package EXPORTHOW { package DECLARE { constant shouter = SomeHOW } }
# The importing unit then parses `shouter Name { ... }` like `class`, the
# declared type's .HOW is an instance of the module's HOW type, and a user
# `compose` override on that HOW runs after registration (here: wrapping
# every local method to uppercase its result). This is the generic mechanism
# behind OO::Monitors' `monitor` declarator.

use DeclareShout;

plan 6;

shouter Greeter {
    method greet() { "hello" }
    method part(Str $name) { "bye, " ~ $name }
}

is Greeter.new.greet, "HELLO",
    'a method of a DECLARE-declared class went through the HOW compose wrap';
is Greeter.new.part("ann"), "BYE, ANN",
    'the compose wrap applies to every local method (args pass through)';
ok Greeter.HOW ~~ MetamodelX::ShoutHOW,
    ".HOW of a DECLARE'd class is an instance of the module's HOW type";
is Greeter.HOW.^name, 'MetamodelX::ShoutHOW',
    '.HOW.^name reports the custom HOW class';

# An ordinary class in the same unit is untouched by the declarator.
class Plain {
    method greet() { "hello" }
}
is Plain.new.greet, "hello", 'a plain class in the same unit is unaffected';
nok Plain.HOW ~~ MetamodelX::ShoutHOW, 'a plain class keeps the native HOW';
