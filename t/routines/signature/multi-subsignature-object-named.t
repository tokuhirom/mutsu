use Test;

plan 4;

# An object destructures by its public attributes as named parts and has no
# positional part, so `C (:$a)` matches it in dispatch and in `.cando`.
class C { has $.a = 'attribute' }

multi describe(C (:$a)) { "c $a" }
multi describe($x) { 'other' }
is describe(C.new), 'c attribute', 'multi candidate with object sub-signature matches';
is describe(42), 'other', 'a non-C argument falls through';
is (-> C (:$a) { $a }).cando(\(C.new)).elems, 1, 'cando accepts the object';
nok (-> C ($first) { $first }).cando(\(C.new)).elems, 'an object has no positional part to unpack';
