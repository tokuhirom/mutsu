use v6;
use Test;

# The WHEREFORE of a declarator pod attached to a lexical `my module` /
# `my package` reports that package's metaclass (Pod::To::Man dispatches on
# `.HOW ~~ Metamodel::ModuleHOW` / `Metamodel::PackageHOW`).

plan 8;

#| A lexical module.
my module LexModule { }
#| A lexical package.
my package LexPackage { }
#| A global module.
module GlobModule { }
#| A global package.
package GlobPackage { }

my @w = $=pod.map(*.WHEREFORE);

is @w[0].^name, 'LexModule', 'a lexical module names itself';
ok @w[0].HOW ~~ Metamodel::ModuleHOW, 'a lexical module reports ModuleHOW';
ok @w[1].HOW ~~ Metamodel::PackageHOW, 'a lexical package reports PackageHOW';
nok @w[1].HOW ~~ Metamodel::ModuleHOW, 'a lexical package is not a module';
ok @w[2].HOW ~~ Metamodel::ModuleHOW, 'a global module reports ModuleHOW';
ok @w[3].HOW ~~ Metamodel::PackageHOW, 'a global package reports PackageHOW';
is $=pod[0].WHY // $=pod[0].contents, 'A lexical module.', 'the pod carries its text';
ok LexModule.HOW ~~ Metamodel::ModuleHOW, 'the module itself reports ModuleHOW';
