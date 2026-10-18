unit module UseInBlockRerun;

sub uibr-inc($x) is export { $x + 1 }
our sub uibr-qualified($x) { $x * 10 }
our $uibr-counter is export = 0;
sub uibr-bump() is export { ++$uibr-counter }
