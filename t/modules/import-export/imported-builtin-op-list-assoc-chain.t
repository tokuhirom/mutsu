use lib $*PROGRAM.parent(3).add('lib');
use Test;

# An imported candidate of a built-in list-associative operator declared
# `is assoc<list>` takes the whole chain `$a (|) $b (|) $c` in one call, and
# keeps the built-in's precedence: the operator's `.prec` is the built-in
# `junctive-or` one, not the default-infix level of a new operator. The
# chain's candidates are bound only in the importing scope, so the chain has
# to run with them in scope; it used to fall through to the built-in union
# (the Range::SetOps distribution's t/04-union.t). Every expected value was
# measured on rakudo.

plan 6;

use ListAssocUnionCandidates;

is (1..2) (|) (3..4) (|) (5..6), 'list(1,3,5)', 'a chain is one list-associative call';
is (1..2) (|) (3..4), 'pair(1,3)', 'two operands take the binary candidate';
is ((1..2) (|) (3..4) (|) (5..6) (|) (7..8)), 'list(1,3,5,7)', 'a longer chain too';
is-deeply 1 (|) 2 (|) 3, Set.new(1, 2, 3), 'operands no candidate takes reach the built-in';
is &infix:<(|)>.prec<dba>, 'junctive-or', 'the operator keeps the built-in precedence';
is &infix:<(|)>.prec<assoc>, 'list', 'and its associativity';
