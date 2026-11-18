# Two EXPORTHOW::DECLARE declarators whose kinds differ only by their HOW:
# `trait-role` builds with a parametric role metaclass, `trait-class` with a
# class metaclass (t/modules/import-export/exporthow-declare-role-how.t).
my class TraitRoleHOW is Metamodel::ParametricRoleHOW { }
my class TraitClassHOW is Metamodel::ClassHOW { }

my module EXPORTHOW {
    package DECLARE {
        constant trait-role = TraitRoleHOW;
        constant trait-class = TraitClassHOW;
    }
}
