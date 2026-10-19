class CanParent {
    method hello { 'hi' }
}

class CanParentHOW is Metamodel::ClassHOW {
    method compose(Mu $type) {
        $type.^add_parent: CanParent;
        callsame;
    }
}

my package EXPORTHOW {
    package DECLARE {
        constant mcp = CanParentHOW;
    }
}
