use Test;

# `Any.reverse` is `self.list.reverse`, so a non-Iterable invocant is a
# one-element list and reverses to itself. mutsu used to flip a Str instead
# (that is `.flip`) and to have no `.reverse` at all on the other scalars.
# `.rotate`, by contrast, is NOT defined on `Any` in Rakudo, so a non-Iterable
# invocant is a method-not-found error rather than a silent `Nil`.

plan 17;

is "abc".reverse.raku, '("abc",).Seq', '.reverse on a Str is a 1-element Seq';
is "abc".flip, 'cba', '.flip is what reverses the characters';
is 42.reverse.raku, '(42,).Seq', '.reverse on an Int';
is (1/2).reverse.raku, '(0.5,).Seq', '.reverse on a Rat';
is 1.5e0.reverse.raku, '(1.5e0,).Seq', '.reverse on a Num';
is True.reverse.raku, '(Bool::True,).Seq', '.reverse on a Bool';
is (1+2i).reverse.raku, '(<1+2i>,).Seq', '.reverse on a Complex';
is (a => 1).reverse.raku, '(:a(1),).Seq', '.reverse on a Pair';
is 100000000000000000000.reverse.elems, 1, '.reverse on a big Int is 1 element';

# The list cases are unchanged.
is [1, 2, 3].reverse.raku, '(3, 2, 1).Seq', '.reverse on an Array still reverses';
is (1..3).reverse.raku, '(3, 2, 1).Seq', '.reverse on a Range still reverses';
is (1, 2).Seq.reverse.raku, '(2, 1).Seq', '.reverse on a Seq still reverses';

# The `reverse(...)` function form already agreed with raku; pin it.
is reverse("abc").raku, '("abc",).Seq', 'reverse() on a Str is a 1-element Seq';
is reverse(1, 2).raku, '(2, 1).Seq', 'reverse() on a list still reverses';

# .rotate is list-only.
is [1, 2, 3].rotate.raku, '(2, 3, 1).Seq', '.rotate on an Array still rotates';
throws-like { 42.rotate }, X::Method::NotFound, '.rotate on an Int is a method-not-found';
throws-like { "ab".rotate }, X::Method::NotFound, '.rotate on a Str is a method-not-found';

done-testing;
