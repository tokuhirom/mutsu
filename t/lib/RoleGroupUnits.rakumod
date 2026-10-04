# Mirrors CSS::Units (CSS::TagSet): a parametric role plus a same-named
# unparametrised role in one module file.
role RoleGroupUnits[$dimension, $units] {
    method dimension { $dimension }
    method units { $units }
    sub postfix:<dpi>(Numeric $v) is export(:dpi) { $v but RoleGroupUnits['res', 'dpi'] }
}
role RoleGroupUnits {
    method value($v) { 1 }
}
