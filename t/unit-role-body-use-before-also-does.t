use v6;
use lib 't/lib';
use Test;

# `use` is compile-time in Raku, so
#
#     unit role R;
#     use Base;
#     also does Base;
#
# must load `Base` before R's composition resolves. mutsu runs a unit-role body at
# role-registration time, which happens *after* the body's `DoesDecl` statements
# are processed, so the body `use` loaded too late and composition died with
# "Unknown role: Base" — even when Base existed. The `unit class` form already
# hoisted its body `use` statements; the `unit role` form did not.
#
# Every role in the PDF::Class distribution is written this way.

plan 6;

use UnitRoleComposer;
use UnitRoleTwoParents;
use UnitClassComposer;

{
    my class C does UnitRoleComposer { }
    is C.new.base-hello, 'base-hello',
        'a method from the role composed via a body `use` + `also does` is present';
    is C.new.own, 'own', 'and the composing role keeps its own methods';
    # By name, so the test does not have to import the parent role itself
    # (importing it here would load it early and mask the bug).
    is-deeply C.^roles.map(*.^name).sort.List,
        ('UnitRoleBase', 'UnitRoleComposer'),
        'the composed parent role is in the type hierarchy';
}

{
    my class D does UnitRoleTwoParents { }
    is D.new.base-hello, 'base-hello', 'two hoisted `use`s: first parent composed';
    is D.new.second, 'second', 'two hoisted `use`s: second parent composed';
}

# The `unit class` counterpart already worked — keep it pinned so the shared
# hoisting stays symmetric.
{
    is UnitClassComposer.new.base-hello, 'base-hello',
        'a `unit class` composing a role through a body `use` still works';
}
