unit module UseInBlockMulti;

proto uibm-f(|) is export {*}
multi uibm-f(Int $x) is export { "M int $x" }
multi uibm-f(Str $x) is export { "M str $x" }
multi uibm-bare(Int $x) is export { "M bare $x" }
sub infix:<+%+>($a, $b) is export { "M $a+%+$b" }
