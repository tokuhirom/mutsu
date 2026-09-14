use nqp;

my class Missing is Int {
    method defined(--> False) { }
}

my sub missing-value() is export {
    nqp::box_i(7, Missing)
}

class GLOBAL::BareModulePrivateTarget {
    method value() { missing-value() }
}
