use Test;

# A `<(` / `)>` marker inside an alternation branch sets the whole match's
# boundaries, as it does inside a `[ … ]` group: a branch shares the enclosing
# capture scope. mutsu's walk dropped the marker when it merged a branch's
# captures, so these matched the whole `xab`. Expected values are raku's.

plan 5;

is ~('xab' ~~ / x [ c || a <( b ] /), 'b', '`<(` in a `||` branch';
is ~('xab' ~~ / x [ c | a <( b ] /), 'b', '`<(` in a `|` branch';
is ~('xab' ~~ / [ c || x a )> ] b /), 'xa', '`)>` in a `||` branch';
is ~('xab' ~~ / [ c | x a )> ] b /), 'xa', '`)>` in a `|` branch';
is 'xab'.subst(/ x [ c || a <( b ] /, 'B'), 'xaB', '`.subst` replaces only the marked part';
