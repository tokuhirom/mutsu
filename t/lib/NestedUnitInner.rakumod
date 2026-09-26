use NestedUnitMiddle;

unit module NestedUnitInner;

my $probe = middle-probe();
our sub nested-probe() is export { $probe }
our sub inner-sees-helper() is export { so try EVAL 'dependency-helper()' }
