# Exports a multi `infix:<+>` of its own, which a role body `use`s
# (t/modules/import-export/role-body-use-keeps-exported-operator.t).
class RoleBodyOps::Provider { }
multi infix:<+>(RoleBodyOps::Provider $a, Real $b) is export { 'provider' }
