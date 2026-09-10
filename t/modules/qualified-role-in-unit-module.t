use v6;
use lib 't/lib';
use Test;

plan 11;

# A role declaration inside a unit module is registered under its qualified
# name. When that declaration composes a qualified parent, a later sibling
# role must still resolve the child's short name in the enclosing package.
# Qualified parents declared in a nested package need the same resolution.
use Issue7861::Unit;

class UsesExternal does Issue7861::Unit::ExternalGrandchild { }
my $external = UsesExternal.new;
is $external.parent, 'parent', 'qualified external role is composed';
is $external.external-child, 'external-child', 'external child role is composed';
is $external.external-grandchild, 'external-grandchild',
    'short-name sibling chain after qualified composition works';

class UsesNested does Issue7861::Unit::NestedGrandchild { }
my $nested = UsesNested.new;
is $nested.nested-parent, 'nested-parent', 'qualified nested role is composed';
is $nested.nested-child, 'nested-child', 'nested child role is composed';
is $nested.nested-grandchild, 'nested-grandchild',
    'short-name nested sibling chain works';

# Keep the non-unit and module-local paths covered as controls for the fix.
module LocalRoleControl {
    role Base { method base { 'base' } }
    role Child does Base { method child { 'child' } }
}
class UsesControl does LocalRoleControl::Child { }
my $control = UsesControl.new;
is $control.base, 'base', 'ordinary module short-name composition remains valid';

role TopLevelParent { method top-level-parent { 'top-level-parent' } }
role TopLevelChild does TopLevelParent { method top-level-child { 'top-level-child' } }
class UsesTopLevel does TopLevelChild { }
is UsesTopLevel.new.top-level-parent, 'top-level-parent',
    'top-level short-name composition remains valid';

role TopLevelQualifiedChild does Issue7861::Parent::R {
    method top-level-qualified-child { 'top-level-qualified-child' }
}
role TopLevelQualifiedGrandchild does TopLevelQualifiedChild {
    method top-level-qualified-grandchild { 'top-level-qualified-grandchild' }
}
class UsesTopLevelQualified does TopLevelQualifiedGrandchild { }
is UsesTopLevelQualified.new.parent, 'parent',
    'qualified composition outside a unit module remains valid';
is UsesTopLevelQualified.new.top-level-qualified-child, 'top-level-qualified-child',
    'the non-unit qualified child role remains composable';
is UsesTopLevelQualified.new.top-level-qualified-grandchild, 'top-level-qualified-grandchild',
    'short-name chain after non-unit qualified composition remains valid';
