unit module TypedSameNameElsewhere;

# Declares typed variables whose names are common in programs (`$s`, `$n`, `$i`),
# for t/types/untyped-slot-ignores-same-named-typed-variable-elsewhere.t
sub typed-s is export { my Int $s = 3; $s ~= 'x' if False; $s }
sub typed-n is export { my Str $n = 'a'; $n }
sub typed-i is export { my Int $i = 1; $i++; $i }
