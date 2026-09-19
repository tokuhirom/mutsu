unit module ExportedAliasReimport;

sub source($value = 'source') is export(:tag) { $value }
our &alias is export(:tag) = &source;
our &partial is export(:tag) = &source.assuming('partial');
