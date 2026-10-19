unit module UseInBlockVars;

our $uibv-scalar is export = 'module';
our @uibv-list is export = <a b>;
our %uibv-hash is export(:hash) = k => 'v';
our $uibv-mandatory is export(:MANDATORY) = 'm';
sub uibv-bump() is export { $uibv-scalar = 'bumped'; @uibv-list.push('c') }
