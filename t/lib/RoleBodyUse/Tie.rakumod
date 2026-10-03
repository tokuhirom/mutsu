# Fixture for t/oo/role/role-body-use-at-declaration.t and
# t/oo/role/role-attribute-custom-trait.t: a role plus an attribute trait whose
# `compose` hook installs an alias accessor (the shape of PDF::COS::Tie's
# `is entry(:alias<...>)`).
unit role RoleBodyUse::Tie;

method tied { 'tied' }

my role AliasHOW {
    has Str $.alias is rw;
    method compose(Mu $package) {
        my $att = self;
        my $name = $att.name.substr(2);
        try $package.^add_method($!alias, method () { "alias of $name: " ~ $att.get_value(self) });
    }
}

multi trait_mod:<is>(Attribute $att, :$aka!) is export {
    $att does AliasHOW;
    $att.alias = $aka;
}

proto sub tie-it(|) is export(:tie-it) {*}
multi sub tie-it(Str $s) { "tied $s" }
multi sub tie-it(Int $i) { "tied int $i" }
