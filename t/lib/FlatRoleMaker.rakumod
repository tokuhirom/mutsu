use v6;
unit module FlatRoleMaker;
use FlatRoleComparable;
use FlatRoleHolder;

sub make-flat-holder() is export {
    return FlatRoleHolder[FlatRoleComparable].new;
}

sub make-flat-item($value) is export {
    my class FlatRoleItem {
        also does FlatRoleComparable[FlatRoleItem];
        has $.value;
        submethod BUILD(:$!value) {}
        method compare-to(FlatRoleItem $other) {
            self.value <=> $other.value;
        }
    }
    return FlatRoleItem.new(value => $value);
}
