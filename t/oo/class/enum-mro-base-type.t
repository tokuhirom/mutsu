use Test;

# An enum type inherits from its base type (#13667): `.^mro` and `.^parents`
# list the base type's chain, as Rakudo's do.

plan 7;

enum E <alpha beta>;
is-deeply E.^mro, (E, Int, Cool, Any, Mu), 'an Int enum';
is-deeply alpha.^mro, (E, Int, Cool, Any, Mu), 'its member';
enum S (x => 's');
is-deeply S.^mro, (S, Str, Cool, Any, Mu), 'a Str enum';
enum N (n1 => 1.5);
is-deeply N.^mro, (N, Rat, Cool, Any, Mu), 'a Rat enum';
enum G (g => [1, 2]);
is-deeply G.^mro, (G, Array, List, Cool, Any, Mu), 'an Array enum';
is-deeply E.^parents, (Int,), '.^parents';
is-deeply Order.^mro, (Order, Int, Cool, Any, Mu), 'a core enum';
