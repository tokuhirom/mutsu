unit role RolePendingTypo::R;

# The `use` sits inside the role body. It is BEGIN-time (it runs when the
# role is declared, see `run_role_body_uses_at_declaration`), so by the time
# `m` is declared nothing can still supply `TotallyBogusTypeName`: it is a
# genuine typo (#8083), reported when the role is declared.
use RolePendingTypo::Helper;

method m(TotallyBogusTypeName:U \x) { 1 }
