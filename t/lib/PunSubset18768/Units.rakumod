# Fixture for t/oo/role/role-pun-my-subset-keeps-role-visible.t: a role group
# (CSS::Units' shape) whose parametric candidate declares a `my subset` and
# whose plain candidate parameterizes it from a method of its pun.
role PunSubset18768::Units[\d, \u] {
    method unit { u }
    my subset Small of PunSubset18768::Units where { True };
}
role PunSubset18768::Units {
    method value($v, $u) { $v but PunSubset18768::Units[1, $u] }
}
