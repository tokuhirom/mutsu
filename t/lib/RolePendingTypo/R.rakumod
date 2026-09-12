unit role RolePendingTypo::R;

# The `use` sits inside the role body (deferred to composition time, see
# `RoleDef::deferred_body`), so the role's own param-type validation at
# registration time cannot yet tell whether `RolePendingTypo::Helper` would
# have supplied `TotallyBogusTypeName`. `role_body_method_decl` accepts the
# parameter optimistically and records the check as pending (#8083); it
# must be genuinely a typo (no module ever supplies this name), so
# `compose_role_into_class` must reject it once the role is actually
# composed and the `use` above has run.
use RolePendingTypo::Helper;

method m(TotallyBogusTypeName:U \x) { 1 }
