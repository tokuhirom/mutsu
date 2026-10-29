unit module MultiReturnSpec;

multi sub definite(Str $s --> 5) is export { 7 }
multi sub definite(Int $i --> 6) is export { 7 }
multi sub flag(Str $s, Int $n --> True) is export { $s.chars }
multi sub typed(Str $s --> Int) is export { "not an int" }

proto sub via-proto(|) is export {*}
multi sub via-proto(Str $s --> 9) { 1 }
