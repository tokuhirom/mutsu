use Test;

# A method of a class declared inside a `module` resolves the module's routines
# by their bare name. Bare-name lookup used to jump straight from the current
# package to GLOBAL, so `NL::Searcher`'s methods could not see `NL`'s subs and
# died with "Unknown function". Now the lookup walks the enclosing packages
# (`NL::Searcher` -> `NL` -> `GLOBAL`), which is what raku's lexical scoping does.

plan 9;

use lib 't/lib';
use ClassSeesModuleSubs;

is ClassSeesModuleSubs::Searcher.try-versions('sqlite3'), 'cn:sqlite3',
    'method sees the module-scope `our sub`';
is ClassSeesModuleSubs::Searcher.via-lexical(4), 12,
    'method sees the module-scope lexical `sub`';
is ClassSeesModuleSubs::Searcher.marked, 'mark',
    'method sees the module-scope `our constant`';
is ClassSeesModuleSubs::Searcher.dispatch('a'), 'str:a',
    'method reaches a module-scope proto/multi (Str candidate)';
is ClassSeesModuleSubs::Searcher.dispatch(7), 'int:7',
    'method reaches a module-scope proto/multi (Int candidate)';
is ClassSeesModuleSubs::Outer::Inner.deep('z'), 'cn:z',
    'a doubly-nested class walks out through both enclosing packages';
is ClassSeesModuleSubs::Nested::Deep.both, 'inner/cn:x',
    'a nested module prefers its own sub and still reaches the outer one';

# A `module { ... }` block in the mainline behaves the same as `unit module`.
module Mainline {
    our sub tag($s) { "t:$s" }
    class Holder {
        method wrap($s) { tag($s) }
    }
}
is Mainline::Holder.wrap('q'), 't:q', 'mainline `module { }` block: class sees the module sub';

# The innermost declaration wins over a same-named GLOBAL one.
sub dup($s) { "global:$s" }
module Shadow {
    our sub dup($s) { "module:$s" }
    class User {
        method call($s) { dup($s) }
    }
}
is Shadow::User.call('v'), 'module:v', 'the enclosing module shadows a same-named GLOBAL sub';
