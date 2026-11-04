use Test;
use lib $*PROGRAM.parent(2).add('lib');
use RwNestedForward;

# An imported routine that forwards its own `is rw` parameter to another
# routine of its module keeps the callee's write, on the first call (the callee
# is not compiled yet) and on every later one (cached dispatch). From TOML's
# `parse-value` -> `parse-integer` chain (#16384).
plan 9;

sub run(&f) { my int $p = 0; f($p); $p }

# Each routine is called twice from a sub: the first call takes the
# compile-on-first-use path, the second the cached one.
is run(&outer-tail), 1, 'tail call, first call';
is run(&outer-tail), 1, 'tail call, second call';
is run(&outer-chain), 1, '// chain, first call';
is run(&outer-chain), 1, '// chain, second call';
is run(&outer-stmt), 1, 'statement call, first call';
is run(&outer-stmt), 1, 'statement call, second call';

sub direct(int $start) { my int $pos = $start; outer-tail($pos); $pos }
is direct(0), 1, 'called by name from a sub, first';
is direct(5), 6, 'called by name from a sub, later';
my int $top = 0; outer-chain($top);
is $top, 1, 'called at file scope';
