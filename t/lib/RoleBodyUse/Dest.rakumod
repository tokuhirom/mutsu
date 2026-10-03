# Fixture for t/oo/role/role-body-use-at-declaration.t: the role's parent, a
# nested role's parent and an attribute trait all come from a module `use`d
# inside the role body (PDF::Destination's shape).
role RoleBodyUse::Dest {
    use RoleBodyUse::Tie;
    also does RoleBodyUse::Tie;

    my role DestDict is export(:DestDict) does RoleBodyUse::Tie {
        has $.page is aka<pg>;
    }
}
