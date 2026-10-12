# A role whose body-level `sub` calls a routine imported into this module
# only, through a role-body constant (#12895).
use RoleBodyImport::Helper;

role RoleBodyImport::Role {
    sub det { role-body-helper() }
    my constant H = det();
    method h { H }
    method direct { det() }
}
