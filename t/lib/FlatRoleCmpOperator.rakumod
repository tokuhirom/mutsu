use v6;
unit module FlatRoleCmpOperator;
use FlatRoleComparable;

multi sub infix:<flat-role-cmp>(Mu $lhs, Mu $rhs) is export {
    return $lhs cmp $rhs;
}
multi sub infix:<flat-role-cmp>(FlatRoleComparable $lhs, FlatRoleComparable $rhs) returns Order:D is export {
    return $lhs.compare-to($rhs);
}
