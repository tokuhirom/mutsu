use v6;
unit module RoleUse::CmpOperator;
use RoleUse::Comparable;

multi sub infix:<role-use-cmp>(Mu $lhs, Mu $rhs) is export {
    return $lhs cmp $rhs;
}
multi sub infix:<role-use-cmp>(RoleUse::Comparable $lhs, RoleUse::Comparable $rhs) returns Order:D is export {
    return $lhs.compare-to($rhs);
}
