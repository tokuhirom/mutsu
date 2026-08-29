unit module ProtoMultiCapture;

# A proto + multi routine, used by t/begin-selective-import-proto-multi.t to
# exercise capturing an imported proto/multi as a first-class `&`-sigil value
# from inside a selectively-scoped import (`my (&f) = do { use ...; &f }`).
proto sub proto-multi-capture($x) is export {*}
multi sub proto-multi-capture(Int $x) { "int:$x" }
multi sub proto-multi-capture(Str $x) { "str:$x" }
