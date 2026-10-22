class DeclareAttrMeta is Attribute {
    method compose(Mu $package, |) {
        die "Attribute {self.name} can't be rw on a frozenattr class" if self.rw;
        callsame
    }
}
class DeclareAttrMetaHOW is Metamodel::ClassHOW { }
my package EXPORTHOW {
    package DECLARE {
        constant frozenattr = DeclareAttrMetaHOW;
        constant frozenattr-attr = DeclareAttrMeta;
    }
}
