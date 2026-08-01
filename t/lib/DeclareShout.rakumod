class MetamodelX::ShoutHOW is Metamodel::ClassHOW {
    method compose(Mu \type) {
        for self.methods(type, :local) -> $m {
            $m.wrap(-> \SELF, | { callsame().uc });
        }
        callsame
    }
}

my package EXPORTHOW {
    package DECLARE {
        constant shouter = MetamodelX::ShoutHOW;
    }
}
