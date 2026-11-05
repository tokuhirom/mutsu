use Test;
use lib $*PROGRAM.parent(2).add('lib-nested-name-type');
use NestedNamePk;

# A nested-name class of a module (`class Window::Clauses` inside
# `unit module NestedNamePk`) written short in an attribute type and in an
# optional parameter's type is the module's `NestedNamePk::Window::Clauses`,
# not a global `Window::Clauses` (SQL::Abstract, #16359).
plan 3;

is NestedNamePk::sel-plain(), 'NestedNamePk::Window::Clauses',
    'an attribute typed with the short nested name accepts the omitted optional';
is NestedNamePk::optional-name(), 'NestedNamePk::Window::Clauses',
    'the omitted optional parameter is the module-qualified type object';

my class A::B { }
sub local-opt(A::B $x?) { $x.^name }
is local-opt(), 'A::B', 'a script-level nested name keeps its spelling';
