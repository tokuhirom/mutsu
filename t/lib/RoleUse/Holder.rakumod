use v6;
unit role RoleUse::Holder[::Type];
use RoleUse::Comparable;
use RoleUse::CmpOperator;

has Type @.nodes;

method insert($value) {
    @!nodes.push($value);
}

method !compare-stored($i, $j) {
    return @!nodes[$i] role-use-cmp @!nodes[$j];
}

method compare-first-two() {
    return self!compare-stored(0, 1);
}
