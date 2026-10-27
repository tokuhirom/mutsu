use Test;

plan 5;

class Marker { }
# A user `infix:<X>` that declines these operands falls back to the core product.
multi infix:<X>(+sources where { sources.elems >= 2 && sources.grep(Marker).elems }) { 'user' }

sub pv { (1, 2) }

is-deeply ((1, 2) X ^2 X <a b>).Array.raku, '[(1, 0, "a"), (1, 0, "b"), (1, 1, "a"), (1, 1, "b"), (2, 0, "a"), (2, 0, "b"), (2, 1, "a"), (2, 1, "b")]',
    'an unparenthesized chain gives flat tuples';
is-deeply (((1, 2) X ^2) X <a b>).Array.raku, '[((1, 0), "a"), ((1, 0), "b"), ((1, 1), "a"), ((1, 1), "b"), ((2, 0), "a"), ((2, 0), "b"), ((2, 1), "a"), ((2, 1), "b")]',
    'a parenthesized left product stays one nested column';
is-deeply ((pv() X ^2) X <a b>).Array.raku, '[((1, 0), "a"), ((1, 0), "b"), ((1, 1), "a"), ((1, 1), "b"), ((2, 0), "a"), ((2, 0), "b"), ((2, 1), "a"), ((2, 1), "b")]',
    'the same with a routine call as the innermost left operand';
is-deeply (<a b> X (pv() X ^2)).Array.raku, '[("a", (1, 0)), ("a", (1, 1)), ("a", (2, 0)), ("a", (2, 1)), ("b", (1, 0)), ("b", (1, 1)), ("b", (2, 0)), ("b", (2, 1))]',
    'a parenthesized right product stays one nested column';
is (Marker.new X 1), 'user', 'the user candidate still wins for its own operands';
