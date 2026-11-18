use lib $*PROGRAM.parent(2).add('lib');
use Test;

# Two proto-token candidates that match the same text tie on LTM, and rakudo
# breaks the tie nearest-first: a role's own candidate before the candidates
# of the roles it composes. The base role here comes from a module, so the
# grammar below is composed twice (its hoisted shell, then the real
# declaration); the second composition re-registered the nearest role's
# candidate with a fresh declaration stamp and the base role's candidate won
# (Lingua::NumericWordForms' Worded-number-specs-parsing-* tests: the
# Armenian `worded_number_100s` answered 0 instead of 100). Every expected
# value was measured on rakudo.

use ProtoTokenBaseRole;

role Derived does ProtoTokenBaseRole {
    regex num:sym<Derived> { 'y' <hund> | <hund> }
}

grammar G does Derived { token TOP { <num> } }

class Actions {
    method num:sym<General>($/) { make 'general' }
    method num:sym<Derived>($/) { make 'derived' }
    method TOP($/) { make $<num>.made }
}

plan 3;

is G.parse('hund', actions => Actions).made, 'derived',
  "the nearest role's candidate wins the tie";
is G.parse('xhund', actions => Actions).made, 'general',
  "the base role's candidate still answers its own text";
is G.parse('yhund', actions => Actions).made, 'derived',
  "and the derived one its own";
