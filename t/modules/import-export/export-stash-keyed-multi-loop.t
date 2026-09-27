use v6;
use lib $?FILE.IO.parent(3).add('lib');
use Test;

# `BEGIN for <&a &b ...> { EXPORT::DEFAULT::{$_} = ::($_) }` re-exporting
# several multis: every candidate of each family arrives, and no family picks
# up another's candidates (#9665 reworked how the aliases are gathered).

plan 9;

use MultiStashReExport;

is fam-one(1), 'one-int 1', 'first family: Int candidate';
is fam-one('x'), 'one-str x', 'first family: Str candidate';
is fam-two(2), 'two-int 2', 'second family: unary candidate';
is fam-two('a', 'b'), 'two-str2 ab', 'second family: binary candidate';
is fam-three(), 'three-0', 'third family: nullary candidate';
is fam-three(1, 2, 3), 'three-n 3', 'third family: slurpy candidate';
is &fam-one.candidates.elems, 2, 'first family has exactly its own candidates';
is &fam-two.candidates.elems, 2, 'second family has exactly its own candidates';
is &fam-three.candidates.elems, 2, 'third family has exactly its own candidates';
