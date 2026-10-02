unit module UnitOurBareName;

# Fixture for t/modules/unit-our-var-not-bound-bare-in-importer.t (#11009).
our $uobn-our = "our";
our $uobn-exp is export = "exp";
our @uobn-arr = 1, 2;
our %uobn-hash = a => 1;

sub uobn-read() is export { "$uobn-our/$uobn-exp/@uobn-arr[]/%uobn-hash<a>" }
sub uobn-bump() is export { $uobn-our ~= "+"; $uobn-our }
