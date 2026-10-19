unit module UseInBlockRerunPlain;

sub uibrp-inc($x) is export { $x + 1 }
our sub uibrp-qualified($x) { $x * 10 }
