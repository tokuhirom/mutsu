unit module RolePendingTypo::Helper;

# Any module the role body `use`s -- it only needs to still be unloaded at
# the role's own registration time, so that `role_body_method_decl`'s
# accept-optimistically branch (registration_role_method.rs) actually
# defers the parameter's type check instead of rejecting it immediately.
sub noop is export { }
