use lib 't/lib';
use Test;

# EXPORTHOW::DECLARE — a `use`d module can register a NEW class-like
# declarator keyword through a constant HOW or a class at the stash entry:
#   my package EXPORTHOW { package DECLARE { constant shouter = SomeHOW } }
# The importing unit then parses `shouter Name { ... }` like `class`, the
# declared type's .HOW is an instance of the module's HOW type, and a user
# `compose` override on that HOW runs after registration (here: wrapping
# every local method to uppercase its result). This is the generic mechanism
# behind OO::Monitors' `monitor` declarator.

use DeclareShout;
use DeclareClassHow;
use DeclareMyClassHow;

plan 13;

shouter Greeter {
    method greet() { "hello" }
    method part(Str $name) { "bye, " ~ $name }
}

is Greeter.new.greet, "HELLO",
    'a method of a DECLARE-declared class went through the HOW compose wrap';
is Greeter.new.part("ann"), "BYE, ANN",
    'the compose wrap applies to every local method (args pass through)';

shouter VersionedGreeter:ver<1.2.3> {
    method greet() { "hello" }
}
is VersionedGreeter.new.greet, "HELLO",
    'a DECLARE class with :ver keeps its HOW compose hook';
is VersionedGreeter.^ver, v1.2.3,
    'a DECLARE class with :ver keeps its version metadata';

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

controller ClassHowHome { }
is ClassHowHome.^name, 'ClassHowHome',
    'a class in EXPORTHOW::DECLARE registers a package declarator';
is ClassHowHome.HOW.^name, 'EXPORTHOW::DECLARE::controller',
    'the class declaration is resolved as the declarator HOW';
is ClassHowHome.new.answer, 42,
    'the class-based declarator runs its custom compose hook';

mycontroller MyClassHowHome { }
is MyClassHowHome.^name, 'MyClassHowHome',
    'a class in my package EXPORTHOW::DECLARE registers a package declarator';
is MyClassHowHome.HOW.^name, 'EXPORTHOW::DECLARE::mycontroller',
    'the lexical package class is resolved as the declarator HOW';
