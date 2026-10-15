unit module UseInBlockM;

sub uib-g() is export { "M g" }
sub uib-named(:$tag = "none") is export { "M named $tag" }
