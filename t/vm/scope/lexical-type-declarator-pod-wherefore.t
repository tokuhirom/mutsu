use Test;

# A lexical (`my`) type's declarator block documents that type: its
# `$=pod` entry's WHEREFORE is the type object itself, and the type's
# `.WHY` is the block (Pod::To::Man renders a `my class` from both).

plan 12;

#| A module doc.
my module TestModule { }
#| A package doc.
my package TestPackage { }
#| c doc
my class TC { }
#| s doc
my subset TS of Int;
class Outer {
    #| inner doc
    my class Inner { }
    method inner { Inner }
}

is $=pod[0].WHEREFORE.HOW.^name, 'Perl6::Metamodel::ModuleHOW', 'a my module documents a module';
ok $=pod[0].WHEREFORE === TestModule, 'and is that module';
is $=pod[1].WHEREFORE.HOW.^name, 'Perl6::Metamodel::PackageHOW', 'a my package documents a package';
ok $=pod[1].WHEREFORE === TestPackage, 'and is that package';
ok $=pod[2].WHEREFORE === TC, 'a my class documents that class';
ok $=pod[3].WHEREFORE === TS, 'a my subset documents that subset';
ok $=pod[4].WHEREFORE === Outer.inner, 'a nested my class documents that class';
is TestModule.WHY, 'A module doc.', 'a my module has its WHY';
is TestPackage.WHY, 'A package doc.', 'a my package has its WHY';
is TC.WHY, 'c doc', 'a my class has its WHY';
is Outer.inner.WHY, 'inner doc', 'a nested my class has its WHY';
is $=pod[2].WHEREFORE.WHY, 'c doc', 'the WHEREFORE answers its own WHY';
