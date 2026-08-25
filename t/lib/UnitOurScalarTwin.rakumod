unit module UnitOurScalarTwin;

# A SECOND module declaring `our $s` under the same bare name. Two packages
# each owning a `$s` must stay completely independent of one another -- the
# bare name alone cannot tell them apart, so the resolution has to go through
# the package the running routine belongs to.

our $s = 'TWIN';

sub twin-read() is export { $s }
sub twin-set($v) is export { $s = $v }
