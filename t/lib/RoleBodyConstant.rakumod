# A `my constant` in a role body read by the role body's own exported subs
# (t/modules/import-export/role-body-constant-for-exported-sub.t, #13279).
role RoleBodyConstant {
    my constant %D = %( a => 1, b => 2 );
    my constant @L = <x y z>;
    sub dimension(\u) is export(:dimension) { %D{u} }
    sub plain() is export(:plain) { %D.elems + @L.elems }
    method via-method { %D<b> }
}
