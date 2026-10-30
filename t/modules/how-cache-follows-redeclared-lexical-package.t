use v6;
use Test;

plan 8;

# A `my enum Foo` in one block must not leave its metaobject behind for a
# later `my module Foo` / `my package Foo` of the same name in another block.
{ my enum Foo <Bar Baz>; is Foo.HOW.^name, 'Perl6::Metamodel::EnumHOW', 'an enum'; }
{ my module Foo { }; is Foo.HOW.^name, 'Perl6::Metamodel::ModuleHOW', 'then a module of that name'; }
{ my package Foo { }; is Foo.HOW.^name, 'Perl6::Metamodel::PackageHOW', 'then a package of that name'; }
{ my class Foo { }; is Foo.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'then a class'; }
{ my module Foo { }; is Foo.HOW.^name, 'Perl6::Metamodel::ModuleHOW', 'a module again'; }

{
    my module Quux { }
    ok Quux.HOW ~~ Metamodel::ModuleHOW, 'a module typechecks against ModuleHOW';
}
{
    my enum Quux <A B>;
    ok Quux.HOW ~~ Metamodel::EnumHOW, 'and the enum of the same name against EnumHOW';
    nok Quux.HOW ~~ Metamodel::ModuleHOW, 'not against ModuleHOW';
}
