use Test;
use lib $*PROGRAM.parent(2).add('lib');

plan 7;

# A routine's free read of a lexical owned by a class or module body hands a
# builtin the variable's value, not the store's shared cell: the cell was one
# opaque item, so `elems($m)` answered 1 and `any($m)` built a one-element
# junction (SortUk's `$c eq any $CHARSET` never matched).
class M {
    my $m = <a b>;
    method info { (elems($m), any($m).raku, reverse($m).join, sort($m).join, join(',', $m)) }
}
is-deeply M.info, (2, 'any("a", "b")', 'ba', 'ab', 'a b'), 'class-body lexical read from a method';

module N {
    my $q = [1, 2];
    our sub info { (elems($q), so(2 == any $q)) }
}
is-deeply N::info(), (2, True), 'module-body lexical read from an our sub';

use PkgLexicalArgs;
ok in-letters('ґ'), 'unit-module constant through any()';
ok in-letters('д'), 'its last element too';
nok in-letters('z'), 'a non-member still fails';
is-deeply pair-info(), (2, 'ba', 'ab'), 'unit-module lexical through elems/reverse/sort';

my $top = <x y>;
sub top-info { elems($top) }
is top-info(), 2, 'a file-scope lexical is unchanged';
