class DeclareAttrMeta is Attribute {
    method compose(Mu $package, |) {
        die "Attribute {self.name} can't be rw on a frozenattr class" if self.rw;
        callsame
    }

    method container_initializer(|) {
        return -> { List.new } if self.name.starts-with('@');
        nextsame
    }
}
class DeclareAttrMetaHOW is Metamodel::ClassHOW { }
my package EXPORTHOW {
    package DECLARE {
        constant frozenattr = DeclareAttrMetaHOW;
        constant frozenattr-attr = DeclareAttrMeta;
    }
}
