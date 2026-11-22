# A DECLAREd role-kind declarator whose HOW's `compose` records the role it
# composes (#18081).
my @composed;
my class TraitRoleComposeHOW is Metamodel::ParametricRoleHOW {
    method compose(Mu \r, *%_) { @composed.push: r.^name; callsame }
}
sub declared-composed() is export { @composed.List }
my module EXPORTHOW { package DECLARE { constant traitrole = TraitRoleComposeHOW; } }
