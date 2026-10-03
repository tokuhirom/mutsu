unit module ModuleScopeAtomicint;

my atomicint $initialized = 0;
my atomicint $hits = 0;
my $plain = 7;

sub first-fetch() is export { ⚛$initialized }
sub plain-read() is export { $initialized }
sub plain-atomic-fetch() is export { ⚛$plain }
sub mark-initialized() is export { $initialized ⚛= 1; ⚛$initialized }
sub hit() is export { $hits⚛++ }
sub hits() is export { ⚛$hits }
