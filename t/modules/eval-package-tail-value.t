use Test;

# A unit whose last statement is a `package` / `module` block is that package's
# type object (`say EVAL('package P6 { 1 }')` is `(P6)`), the way a trailing
# `class` already was; it answered Nil (#12086). Every expected answer is
# Rakudo 2026.09's.

plan 17;

is EVAL('package P6 { 1 }').raku, 'P6', 'a package';
is EVAL('module M6 { 1 }').raku, 'M6', 'a module';
is EVAL('package P7 { enum E <A B> }').raku, 'P7', 'a package whose last statement is an enum';
is EVAL('class C6 { }').raku, 'C6', 'a class (the control that already worked)';

is EVAL('package A::B { 1 }').raku, 'A::B', 'a qualified name';
is EVAL('my package MP { 1 }').raku, 'MP', 'a `my package`';
is EVAL('our package OP { 1 }').raku, 'OP', 'an `our package`';
is EVAL('module M8 { sub f is export { 3 } }').raku, 'M8', 'a module that exports';
is EVAL('package P10 { package Inner { 1 } }').raku, 'P10', 'the outer of two nested packages';
is EVAL('role R6 { }').raku, 'R6', 'a role (control)';
is EVAL('grammar G6 { token TOP { x } }').raku, 'G6', 'a grammar (control)';
is EVAL('package P11 { our $x = 5 }').^name, 'P11', 'the type object names itself';
nok EVAL('package P16 { 1 }').defined, 'and is a type object, not an instance';

# The value of the unit is the LAST statement's, and the package still exists.
is EVAL('package P9 { }; 42'), 42, 'a package followed by an expression';
is EVAL('package P12 { }; package P13 { }').raku, 'P13', 'two packages: the last one';
is EVAL('package PV { our sub f { 3 } }; PV::f()'), 3, 'the package is declared and usable';
is EVAL('package PW { 1 } # trailing comment').raku, 'PW', 'a trailing comment after the block';
