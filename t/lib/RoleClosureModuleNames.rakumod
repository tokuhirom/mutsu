module RoleClosureModuleNames {
    # Deliberately NOT exported: only code written in this module can name it.
    role Tag { }
    multi sub trait_mod:<is>(Method $m, :$tagged!) is export { $m does Tag }

    role Holder is export {
        has $.v;
        method !v() is rw { $!v }
        method v(Holder:D $SELF:) is rw {
            Proxy.new(
                FETCH => method () { $SELF!v },
                STORE => method ($val) { $SELF.store($val) },
            );
        }
        method closure() { -> { Tag.^name } }
        method tagged-count() { self.^methods.grep(Tag).elems }
        method store($val) {
            $!v = self.tagged-count ~ ':' ~ $val;
        }
    }
}
