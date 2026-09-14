use NestedUnitDependency;

sub EXPORT() { Map.new: '&middle-marker' => sub { 'middle' } }

unit module NestedUnitMiddle;
