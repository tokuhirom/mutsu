# An EXPORTHOW::DECLARE declarator the module binds at BEGIN time rather than
# declaring it in `package DECLARE` (t/modules/import-export/exporthow-declare-role-how.t).
my class GizmoHOW is Metamodel::ClassHOW { }
my class GadgetRoleHOW is Metamodel::ParametricRoleHOW { }
my module EXPORTHOW { package DECLARE { } }
BEGIN EXPORTHOW::DECLARE.WHO<gizmo> := GizmoHOW;
BEGIN EXPORTHOW::DECLARE.WHO<gadget> := GadgetRoleHOW;
