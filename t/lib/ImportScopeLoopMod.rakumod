unit module ImportScopeLoopMod;

sub isl-plain($x) is export { "plain:$x" }
proto sub isl-multi(|) is export {*}
multi sub isl-multi(Int $x) is export { "int:$x" }
multi sub isl-multi(Str $x) is export { "str:$x" }
