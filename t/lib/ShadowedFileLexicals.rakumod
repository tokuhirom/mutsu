# File-scope containers whose names a routine's own `my` reuses.
unit module ShadowedFileLexicals;

my @log;
my %seen;

sub push-own is export { my @log; @log.push(1); @log.push(2); @log.elems ~ '/' ~ @log[1] }
sub key-own is export { my %seen; %seen<a> = 1; %seen.elems }
sub index-own is export { my @log; @log[0] = 5; @log.elems }
sub typed-key-own($k) is export { my %seen{Str}; %seen{$k} = 1; %seen.keys.join(',') }
sub file-sizes is export { @log.elems ~ '/' ~ %seen.elems }
sub file-store is export { @log[0] = 'f'; %seen<f> = 1; file-sizes() }
