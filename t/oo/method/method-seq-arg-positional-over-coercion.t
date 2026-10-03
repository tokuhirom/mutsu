use Test;

plan 4;

# A Seq binds to `@arr` through PositionalBindFailover, which makes the `@`
# candidate narrower than a `Str()` coercion candidate (Trie's `delete`).
class D {
    multi method d(@arr)    { "arr {@arr.elems}" }
    multi method d(Str() $k) { "str $k" }
}

is D.d("ab".comb), 'arr 2', 'a Seq picks the @ candidate';
is D.d([1, 2]), 'arr 2', 'an Array picks the @ candidate';
is D.d("x"), 'str x', 'a Str picks the coercion candidate';
is D.d(5), 'str 5', 'an Int is coerced';
