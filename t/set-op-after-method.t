# A `(...)`-delimited set/baggy infix operator immediately after a method call,
# with a space before it (`@a.Set (|) @b.Set`), is an operator — not a
# space-separated call-arg list. mutsu erroneously rejected the space with
# "no space allowed between method name and the left parenthesis".
use Test;

plan 10;

is ((1,2,3).Set (|) (3,4).Set).keys.sort.join(","), '1,2,3,4', 'union (|) after .Set';
is ((1,2,3).Set (&) (2,3,4).Set).keys.sort.join(","), '2,3', 'intersection (&) after .Set';
is ((1,2,3).Set (-) (2).Set).keys.sort.join(","), '1,3', 'difference (-) after .Set';
is ((1,2).Set (^) (2,3).Set).keys.sort.join(","), '1,3', 'symmetric diff (^) after .Set';
ok ((1,2).Set (<=) (1,2,3).Set), 'subset (<=) after .Set';
ok ((1,2,3).Set (>) (1,2).Set), 'strict superset (>) after .Set';
ok (2 (elem) (1,2,3).Set), '(elem) before .Set';
ok ((1,2,3).Set (cont) 2), '(cont) after .Set';
is ((1,2).Bag (+) (2,3).Bag).total, 4, 'baggy union (+) after .Bag';

# A genuine space-before-call-paren is still an error.
ok (try { EVAL 'my @a = 1, 2; @a.map (1)' } === Nil and $!.defined),
    'real "space before call paren" still rejected';
