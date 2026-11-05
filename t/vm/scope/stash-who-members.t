use Test;
plan 8;

# Pins which registered names a package's stash lists (`.WHO`, `P::`), the
# question `stash_member_tail` answers for every class/role/package: a
# `Package::rest` key, or the last `::Package::rest` inside a longer key.
package Outer { class Inner { }; our sub f() { 1 } }
class Plain { }
module Mod::Sub { class Deep { } }

ok Outer::.keys.grep('Inner'), 'Outer:: lists its class Inner';
ok Outer::.keys.grep('&f'), 'Outer:: lists its our-sub';
ok Mod::.keys.grep('Sub'), 'Mod:: lists the sub-package Sub';
ok Mod::Sub::.keys.grep('Deep'), 'Mod::Sub:: lists Deep';
nok Plain::.keys.grep('Inner'), 'Plain:: does not list another package\'s class';
is 42.WHO.^name, 'Stash', '42.WHO is a Stash';
is Int.WHO.gist, Int::.gist, 'Int.WHO is the Int:: stash';
nok Outer::.keys.grep('Plain'), 'Outer:: does not list a sibling at top level';
