use Test;

plan 4;

# A Seq binds to an `@` parameter (PositionalBindFailover), and that candidate
# is narrower than an `Any:D` one, as in rakudo.
multi sub h(@raw, %n) { "list {@raw.elems}" }
multi sub h(Any:D $spec, %n) { "any" }
is h("a b".words, %()), 'list 2', 'a Seq picks the @ candidate over Any:D';
is h(<a b c>, %()), 'list 3', 'a List too';
is h("x", %()), 'any', 'a Str still takes Any:D';

multi sub k(Any:D $x) { "any" }
multi sub k(@x) { "list" }
is k((1, 2).map(* + 1)), 'list', 'regardless of declaration order';
