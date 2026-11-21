use v6;
use lib 't/lib';
use Test;

# A package block declared inside a routine (directly, or in code the routine
# EVALs) is named under the package the routine was declared in, and its
# `$?PACKAGE` is that package block. mutsu folded `$?PACKAGE` to the routine's
# enclosing package and left the block unqualified (#18480).

plan 7;

sub top-level { module PkgInTopSub { our sub name { $?PACKAGE.^name } }; PkgInTopSub::name() }
is top-level(), 'PkgInTopSub', '$?PACKAGE in a package block inside a sub';

module Outer18480 {
    our sub direct { module Inner { our sub name { $?PACKAGE.^name } }; Inner::name() }
    our sub evald { EVAL q[module Evald { our sub name { $?PACKAGE.^name } }; Evald::name()] }
    our sub evald-here { EVAL q[$?PACKAGE.^name] }
}
is Outer18480::direct(), 'Outer18480::Inner', 'a module in a module routine is qualified';
is Outer18480::evald(), 'Outer18480::Evald', 'a module EVALed from a module routine is qualified';
is Outer18480::evald-here(), 'Outer18480', '$?PACKAGE in the EVAL itself is the routine package';
is Outer18480::Inner::name(), 'Outer18480::Inner', 'the nested module is reachable qualified';

use LexPkgShadowCompiler;
is compile-it(q[module FromLoaded { our sub name { $?PACKAGE.^name } }; FromLoaded::name()]),
    'LexPkgShadowCompiler::FromLoaded', 'a module EVALed from a loaded module routine';

class WithMethod18480 { method m { package P { our sub name { $?PACKAGE.^name } }; P::name() } }
is WithMethod18480.m, 'WithMethod18480::P', 'a package in a method is qualified by the class';
