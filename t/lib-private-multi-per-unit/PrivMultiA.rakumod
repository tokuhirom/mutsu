unit module PrivMultiA;

# A module-private multi named like the one in PrivMultiB: each module's calls
# must reach its own candidates.
my proto sub pick(|) {*}
my multi sub pick(Int $x) { "A.Int($x)" }
my multi sub pick(Str $x) { "A.Str($x)" }
my multi sub pick(Int $x, Int $y) { "A.Int2($x,$y)" }

sub a-pick-int($x) is export { pick($x) }
sub a-pick-str($x) is export { pick($x) }
sub a-pick-two($x, $y) is export { pick($x, $y) }
