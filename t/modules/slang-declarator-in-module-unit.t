use lib 't/lib';
use Test;

# A package declarator a slang adds, used inside a module unit that `use`s the
# slang (#16888, EEL 0.3's precompiled-module tests: `unit module X; use EEL;
# our event Flow`): the declared package is built with the declarator's
# metaclass there too, not with a plain ClassHOW.

plan 3;

use GizmoUser;

is GizmoUser::Thing.HOW.^name, 'MetamodelX::GizmoHOW', 'the module declares with the slang metaclass';
is GizmoUser::Thing.^gizmo-tag, 'gizmo:GizmoUser::Thing', 'its methods answer';
is GizmoUser::Thing.new.hello, 'hi', 'the package works';
