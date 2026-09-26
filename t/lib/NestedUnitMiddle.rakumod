use NestedUnitDependency;

sub EXPORT() {
    Map.new:
        '&middle-marker' => sub { 'middle' },
        '&middle-probe'  => sub { dependency-helper() }
}

unit module NestedUnitMiddle;
