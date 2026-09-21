unit class UnitClassUseBeforeDeclOperator::Provider;

has Int $.value;

method !value(--> Int) {
    $.value
}

sub infix:<☈×>(UnitClassUseBeforeDeclOperator::Provider:D $left,
                UnitClassUseBeforeDeclOperator::Provider:D $right --> Int) is export {
    $left!value * $right!value
}
