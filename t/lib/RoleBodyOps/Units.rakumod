# The CSS::Units shape (#14161): one role candidate exports an operator, and
# another candidate of the same role `use`s a module exporting a candidate of
# that operator too.
role RoleBodyOps::Units[\units] {
    method type { units }
    sub postfix:<upt>(Numeric $v) is export(:pt) { $v but RoleBodyOps::Units['pt'] }
    multi sub infix:<+>(RoleBodyOps::Units $v, RoleBodyOps::Units $n) is default is export(:ops) {
        ($v - -$n) but RoleBodyOps::Units[$v.type]
    }
}
role RoleBodyOps::Units {
    use RoleBodyOps::Provider;
    method plain { 'plain' }
}
