unit module ModuleUnitBind::Readonly;

# A file-scope readonly binding of the module's own.
my $mub-ro := 'bound';

sub ro-reads() is export { $mub-ro }
