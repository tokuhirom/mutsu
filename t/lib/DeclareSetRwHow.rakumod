class DeclareSetRwHOW is Metamodel::ClassHOW {
    method set_rw(Mu \type) { die "frozen class {type.^name} can't be rw" }
}

my package EXPORTHOW {
    package DECLARE {
        constant frozen = DeclareSetRwHOW;
    }
}
