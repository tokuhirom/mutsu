# Mirrors AttrX::Mooish 1.0.10 t/lib/CustomHOW.rakumod (#14241).
class CustomClassHOW is Metamodel::ClassHOW { }
class CustomRoleHOW is Metamodel::ParametricRoleHOW { }
my package EXPORTHOW {
    package SUPERSEDE {
        constant class = CustomClassHOW;
        constant role = CustomRoleHOW;
    }
}
