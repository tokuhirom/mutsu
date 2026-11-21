use v6;
use Test;

# A type declared inside a module or package block is a lexical of that block
# under its short name, so there it shadows a same-named outer type; outside
# the block the outer type is untouched. mutsu bound the short name only for a
# type nested in a class, so in a module the bare name kept meaning the outer
# type (#18481).

plan 10;

class G { }

module ModOur { our class G { }; our sub inner { G.^name } }
is ModOur::inner(), 'ModOur::G', 'our class in a module shadows the outer type';

module ModPlain { class G { }; our sub inner { G.^name } }
is ModPlain::inner(), 'ModPlain::G', 'a plain class in a module shadows the outer type';

module ModMy { my class G { }; our sub inner { G.^name } }
is ModMy::inner(), 'ModMy::G', 'a my class in a module shadows the outer type';

module ModPkg { my package G { }; our sub inner { G.^name } }
is ModPkg::inner(), 'ModPkg::G', 'a my package in a module shadows the outer type';

module ModRole { role G { }; our sub inner { G.^name } }
is ModRole::inner(), 'ModRole::G', 'a role in a module shadows the outer type';

module ModParent { class G { }; class H is G { }; our sub inner { H.^parents[0].^name } }
is ModParent::inner(), 'ModParent::G', 'a parent named in the module is the module\'s own type';

is G.^name, 'G', 'outside the modules the outer type is untouched';
is ModPlain::G.^name, 'ModPlain::G', 'the nested type is still reachable qualified';

class Other::G { }
module Other { our sub inner { G.^name } }
is Other::inner(), 'G', 'a type declared outside the block under its package does not shadow';

module Fresh { class Lonely { } }
throws-like { EVAL 'Lonely' }, X::Undeclared::Symbols, 'the short name does not leak out of the block';
