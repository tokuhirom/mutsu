unit module PrivMultiB;

my proto sub pick(|) {*}
my multi sub pick(Int $x) { "B.Int($x)" }
my multi sub pick(Str $x) { "B.Str($x)" }
my multi sub pick(Int $x, Int $y) { "B.Int2($x,$y)" }

sub b-pick-int($x) is export { pick($x) }
sub b-pick-str($x) is export { pick($x) }
sub b-pick-two($x, $y) is export { pick($x, $y) }
