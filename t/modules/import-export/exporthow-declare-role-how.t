use lib 't/lib';
use Test;

# An EXPORTHOW::DECLARE declarator whose HOW builds a parametric role
# (`is Metamodel::ParametricRoleHOW`) parses like `role`: the declaration
# takes a `[...]` signature and the type composes into a class with `does`.
# The parser learns that from the HOW object of the loaded module
# (ADR-17558 slice 3); the module's source names only the HOW's class.
# A declarator the module binds at BEGIN time is one of its declarators too.

use DeclareRoleHow;
use DeclareBoundHow;

plan 10;

trait-role Greets[$who] {
    method hi { "hi $who" }
}

trait-class Person does Greets["bob"] {
    method name { "Person" }
}

is Person.new.hi, 'hi bob', 'a role-HOW declarator takes a parametric signature';
is Person.new.name, 'Person', 'a class-HOW declarator of the same module declares a class';
ok Person ~~ Greets, 'the class does the role the declarator built';
is Greets[42].^name, 'Greets[Int]', 'the role curries over its signature';
is Greets.^candidates[0].HOW.^name, 'TraitRoleHOW',
    "the role is built with the declarator's HOW";
is Person.HOW.^name, 'TraitClassHOW', "the class is built with its declarator's HOW";

trait-role Plain {
    method plain { 'plain' }
}
is (class { also does Plain }).new.plain, 'plain',
    'a role-HOW declarator without a signature declares a plain role';

gadget Sized[Int $n] { method size { $n } }
gizmo Box does Sized[3] { }
is Box.new.size, 3, 'declarators a module binds at BEGIN time are imported';
is Box.HOW.^name, 'GizmoHOW', "a bound class declarator builds with its HOW";
is Sized.^candidates[0].HOW.^name, 'GadgetRoleHOW', "a bound role declarator builds with its HOW";
