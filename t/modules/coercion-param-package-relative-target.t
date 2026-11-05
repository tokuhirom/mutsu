use Test;
use lib 't/lib';
use CoerceRelPkg;

plan 4;

# `Column::List(Any)` is written relative to `unit class CoerceRelPkg`; the
# routine is called from outside that package.
is CoerceRelPkg::s(5), 'CoerceRelPkg::Column::List', 'positional coercion target resolves in the declaring package';
is CoerceRelPkg::u(), 'CoerceRelPkg::Column::List', 'a coerced default resolves too';
is CoerceRelPkg::t(:c(5)), 'CoerceRelPkg::Column::List', 'named coercion';
lives-ok { CoerceRelPkg::t() }, 'an omitted coercion-typed named param binds a type object the typed attribute accepts';
