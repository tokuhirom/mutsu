unit module UseInBlockOps;

sub infix:<+%+>($a, $b) is export { "M($a,$b)" }
sub uibo-j($x) is export { "j($x)" }
