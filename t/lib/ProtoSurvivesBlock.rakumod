unit module ProtoSurvivesBlock;

our proto sub psb-it(|) is export {*}
our multi sub psb-it(Str $x) is export { "it:$x" }
our multi sub psb-it(Int $x) is export { "int:$x" }

our sub psb-plain() is export { 'plain' }
