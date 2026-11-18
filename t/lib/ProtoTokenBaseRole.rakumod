# A base grammar role with a proto token and one candidate, for
# t/grammar/proto-token-role-candidate-tie-order.t (the shape of
# Lingua::NumericWordForms' WordedNumberSpec roles).
role ProtoTokenBaseRole {
    proto token num {*}
    regex num:sym<General> { 'x' <hund> | <hund> }
    token hund { 'hund' }
}
