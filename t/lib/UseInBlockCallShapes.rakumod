unit module UseInBlockCallShapes;

sub uics-add($a, $b = 10) is export { $a + $b }
sub uics-none() is export { 'none' }
sub uics-named(:$x = 1) is export { $x * 2 }
sub uics-rw($v is rw) is export { $v = $v * 3 }
sub uics-all(*@xs) is export { @xs.join('-') }
sub uics-list(@xs) is export { @xs.elems }
