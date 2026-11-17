use v6;
use lib 't/lib-bareword-package-relative';
use Test;

# A bareword naming a type of the module's own package (`Cell` in
# `unit module BwRel::Mod`, resolving to `BwRel::Mod::Cell`) is remembered per
# site together with the running-package context it was resolved under. The
# same routine reached with another running package (a method of a class that
# declares its own `Cell`) must still resolve the name afresh.

use BwRel::Mod;

plan 8;

is BwRel::Mod::make-cell().kind, 'module cell', 'module routine: the module class';
is BwRel::Mod::make-cell().kind, 'module cell', 'again, from the memo';
is BwRel::Mod::Grid.new.own-cell.kind, 'grid cell', 'a method of Grid: its nested class';
is BwRel::Mod::Grid.new.own-cell.kind, 'grid cell', 'again';
is BwRel::Mod::make-cell().kind, 'module cell', 'the module routine after the method';
is BwRel::Mod::Sheet.new.role-cell.kind, 'module cell', 'a role method composed into a class';
is-deeply BwRel::Mod::make-many(3), ('module cell' xx 3).List, 'a closure in a loop';
is BwRel::Mod::Grid.new.make-cell.kind, 'module cell', 'a module routine called from a method of Grid';
