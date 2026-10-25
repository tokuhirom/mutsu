sub make-reader($attr, $name) {
    my method (Mu:U \SELF:) {
        die "no flag" without $*FLAG;
        "{ $attr.name }/$name/{ SELF.^name }"
    }
}

multi trait_mod:<is>(Attribute:D $attr, :$reader!) is export {
    my $name = $attr.name.substr(2);
    $attr.package.^add_multi_method: $name, make-reader($attr, $name);
}
