use Test;

# A `Seq` argument binds to an `@` parameter through PositionalBindFailover,
# so in multi-METHOD dispatch it ranks as a List, exactly as multi-sub dispatch
# already ranked it. It used to rank as unrelated to Positional, so
# `(Str() $key)` won and Trie's `delete(Str() $key) { self.delete: $key.comb }`
# re-coerced the Seq to a growing string until memory ran out.

plan 5;

class N {
    multi method d(@arr) { "array {@arr.elems}" }
    multi method d(Str() $k) { "str $k" }
    multi method e(@arr) { 'array' }
    multi method e(Any $k) { 'any' }
}

is N.new.d('abc'.comb), 'array 3', 'a Seq picks (@arr) over (Str() $k)';
is N.new.d((1, 2).map(* + 1)), 'array 2', 'a mapped Seq too';
is N.new.d('xyz'), 'str xyz', 'a Str still picks the coercion candidate';
is N.new.e('abc'.comb), 'array', 'a Seq picks (@arr) over (Any $k)';

multi sub f(@a) { 'array' }
multi sub f(Str() $k) { 'str' }
is f('ab'.comb), 'array', 'multi-sub dispatch agrees';
