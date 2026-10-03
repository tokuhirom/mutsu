use Test;

plan 6;

# A Pair held in a variable is a positional argument of `\($pair)`, so
# `.cando` must match it against a positional `Pair` parameter.
my &takes-pair = -> Pair $p { 1 };
for (:d(4),) -> $x {
    is &takes-pair.cando(\($x)).elems, 1, 'cando matches a Pair loop variable positionally';
}
is &takes-pair.cando(\((d => 4))).elems, 1, 'cando matches a parenthesised Pair positionally';
is &takes-pair.cando(\(:d(4))).elems, 0, 'a named argument does not match a positional Pair';

# A rename onto a destructuring target (`:value([$b, $c])`) unpacks the value
# into the target, not the value itself one level deeper.
my &unpack = -> Pair (:key($a), :value([$b, $c])) { "$a: {$b + $c}" };
is &unpack.cando(\((e => [5, 6]))).elems, 1, 'cando accepts a Pair whose value unpacks';
is &unpack.cando(\((d => 4))).elems, 0, 'cando rejects a Pair whose value does not unpack';
multi unpack-multi(Pair (:key($a), :value([$b, $c]))) { "$a: {$b + $c}" }
multi unpack-multi($x) { 'other' }
is unpack-multi((e => [5, 6]).item), 'e: 11', 'multi dispatch picks the destructuring candidate';
