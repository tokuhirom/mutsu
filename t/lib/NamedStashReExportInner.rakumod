unit module NamedStashReExportInner;

sub tags (:$array = False) is export(:DEFAULT) { %( project => 'demo' ) }
sub shout ($s) is export { $s.uc }
