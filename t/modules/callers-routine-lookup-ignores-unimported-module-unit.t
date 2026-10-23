use v6;
use lib $?FILE.IO.parent(2).add('lib');
use Test;
use CallersScopeHub;
use CallersScopeProbe;

plan 3;

# `CALLERS::<&name>` walks the lexical scope of the calling routine. The program
# imported `&__scope_marker` through a `sub EXPORT` hook, but a routine of a
# module unit that imported nothing does not have it in scope, even though the
# program called into it.
is-deeply probe(), (True, False), 'the program mainline has the import in its own pad';
is-deeply Prober.new.ask, (False, False), 'a routine of a unit that imported nothing does not';
sub inner { probe() }
is-deeply inner(), (True, True), 'a routine of the importing unit does';
