use NestedUnitMiddle;

unit module NestedUnitInner;

my $probe = dependency-helper();
our sub nested-probe() is export { $probe }
