use v6;
unit role FlatRoleHolder[::Type];
use FlatRoleComparable;
use FlatRoleCmpOperator;

has Type @.nodes;

method insert($value) {
    @!nodes.push($value);
}

method !compare-stored($i, $j) {
    return @!nodes[$i] flat-role-cmp @!nodes[$j];
}

method compare-first-two() {
    return self!compare-stored(0, 1);
}
