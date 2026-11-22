# A role HOW whose `compose` records the role it composes, installed for the
# importing unit's `role` declarator (#18081).
my @composed;
my class ComposeLoggingRoleHOW is Metamodel::ParametricRoleHOW {
    method compose(Mu \r, *%_) { @composed.push: r.^name; callsame }
}
sub composed-roles() is export { @composed.List }
my module EXPORTHOW { package SUPERSEDE { constant role = ComposeLoggingRoleHOW; } }
