our $SEEN;

class FindMethodComposeHOW is Metamodel::ClassHOW {
    method compose(Mu \type) {
        $SEEN = type.^find_method("WHICH", :local).defined;
        type.^add_method: "WHICH", method () { ValueObjAt.new("added|" ~ self.^name) };
        nextsame
    }
}

my package EXPORTHOW {
    package DECLARE {
        constant composed-how = FindMethodComposeHOW;
    }
}
