# Two registry roles that both name their type capture `::Constraint` and keep
# a role-body `my` variable, reduced from MUGS::Util::ImplementationRegistry.
role RoleCaptureSmiley::ImplRegistry[::Constraint] {
    my %impl;
    method register-implementation(Constraint:U $class) { %impl<x> = $class; 'impl' }
    method register-defined(Constraint:D $obj) { 'impl-defined' }
}

role RoleCaptureSmiley::UIRegistry[::Constraint] {
    my %ui;
    method register-ui(Constraint:U $class) { %ui<x> = $class; 'ui' }
}
