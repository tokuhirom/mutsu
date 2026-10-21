unit module TraitMixinOuter::Raw;
role Marked { method CALL-ME(|c) { "called-me" } }
multi sub trait_mod:<is>(Routine $r, :$marked!) is export { $r does Marked }
our sub compress($a) is marked is export { "raw" }
sub inner-call { compress(1) ~ "/" ~ &compress(1) }
our sub probe is export { inner-call() }
