use Test;

plan 5;

# Found with Kind 1.0.3 t/01-typecheck.t (#15520): the same name declared as a
# different lexical package kind in sibling blocks reports each declaration's
# own metaclass, not the one the first declaration cached.

{ my grammar Foo { token TOP { <?> } }; is Foo.HOW.^name, 'Perl6::Metamodel::GrammarHOW', 'grammar'; }
{ my module Foo { };  is Foo.HOW.^name, 'Perl6::Metamodel::ModuleHOW', 'module after grammar'; }
{ my class Foo { };   is Foo.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'class after module'; }
{ my package Foo { }; is Foo.HOW.^name, 'Perl6::Metamodel::PackageHOW', 'package after class'; }
{ my module Foo { };  is Foo.HOW.^name, 'Perl6::Metamodel::ModuleHOW', 'module after package'; }
