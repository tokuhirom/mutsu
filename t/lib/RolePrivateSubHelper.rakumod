unit role RolePrivateSubHelper;

# A private (non-`is export`) top-level sub in a role body must be callable
# from an exported sub of the SAME role the moment the role is declared --
# not only after some class composes the role. See
# t/modules/import-export/role-body-private-sub-called-from-export.t.
sub greet-private(Str $name) {
    return "hello, $name";
}

sub greet(Str $name) is export {
    return greet-private($name);
}
