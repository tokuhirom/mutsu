use v6;
unit module RoleUseMaker;
use RoleUse::Comparable;
use RoleUse::Holder;

sub make-holder() is export {
    return RoleUse::Holder[RoleUse::Comparable].new;
}

sub make-item($value) is export {
    my class Item {
        also does RoleUse::Comparable[Item];
        has $.value;
        submethod BUILD(:$!value) {}
        method compare-to(Item $other) {
            self.value <=> $other.value;
        }
    }
    return Item.new(value => $value);
}
