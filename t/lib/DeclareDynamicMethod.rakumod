class DynMethodHOW is Metamodel::ClassHOW is export {
    method add-reader(::T Mu:U \type, Str $name) {
        type.^add_multi_method: $name, method (T:U:) {
            die "no flag" without $*FLAG;
            "read-$name"
        }
    }
    method compose(Mu \type, |) {
        self.add-reader(type, "a");
        callsame;
    }
}

my package EXPORTHOW { package DECLARE { constant dynmodel = DynMethodHOW; } }
