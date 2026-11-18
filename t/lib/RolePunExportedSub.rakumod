# A role group whose plain role's body exports a sub that reads the body's
# `my constant` (CSS::Units' `dimension`, #18106).
role RolePunExportedSub[\d, \u] { method type { u } }
role RolePunExportedSub {
    my constant %D = %( px => 0.75 );
    sub pun-dim(\u) is export(:pun-dim) { %D{u} }
    method value(\v, \u) { v but RolePunExportedSub[pun-dim(u), u] }
}
