use UnitClassUseBeforeDeclOperator::Provider;

unit class UnitClassUseBeforeDeclOperator::Consumer;

method apply(--> Int) {
    self!calculate
}

method !calculate(--> Int) {
    UnitClassUseBeforeDeclOperator::Provider.new(value => 3)
        ☈× UnitClassUseBeforeDeclOperator::Provider.new(value => 4)
}
