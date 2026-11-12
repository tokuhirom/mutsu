use Test;
use lib 't/fixtures/exporthow-imported-how/lib';
use GizmoDecl;

# A declarator from `EXPORTHOW::DECLARE` (or a slang, e.g. EEL's `event`) with a
# `my`/`our` scope and an `is export` trait still builds its package with the
# declarator's HOW: the export wrapper used to hide the declaration from the
# mark that names the declarator, so the class came out a plain ClassHOW.
# Identical under rakudo.
plan 4;

our gizmo A is export { method hi { "a" } }
my gizmo B is export { method hi { "b" } }
our gizmo C { }

is A.HOW.^name, 'MetamodelX::GizmoHOW', 'our + is export keeps the declarator HOW';
is B.HOW.^name, 'MetamodelX::GizmoHOW', 'my + is export keeps the declarator HOW';
is C.HOW.^name, 'MetamodelX::GizmoHOW', 'our without traits keeps it too';
is A.hi, 'a', 'and the class body is intact';
