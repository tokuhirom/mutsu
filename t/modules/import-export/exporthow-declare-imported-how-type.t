use Test;
use lib 't/fixtures/exporthow-imported-how/lib';
use GizmoDecl;

# `constant kw = SomeHOW` in `EXPORTHOW::DECLARE`, where the HOW is imported from
# another module (not declared in the exporting file), still exports the
# declarator. Identical under rakudo.
plan 2;

gizmo Foo { method hi { "hi" } }
is Foo.hi, "hi", 'the declarator imported through a module that uses the HOW and an EXPORT-hook module';
is Foo.HOW.^name, 'MetamodelX::GizmoHOW', 'and the HOW is the imported one';
