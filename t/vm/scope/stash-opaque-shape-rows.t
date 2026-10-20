use Test;

# A Stash answers its own rows through an opaque dispatch shape, and its
# associative indexing stays with the hash paths.
plan 8;

is GLOBAL::.^can('merge-symbols').elems, 1, 'a stash has merge-symbols';
is MY::.^can('merge-symbols').elems, 0, 'a PseudoStash does not';
is GLOBAL.WHO.^name, 'Stash', 'GLOBAL.WHO is a Stash';

class Foo { our $x = 5 }
is Foo::<$x>, 5, 'a stash is indexed by its symbols';
is Foo::.keys.sort.join(','), '$x', 'a stash lists its keys';
ok Foo.WHO.^can('merge-symbols'), 'a package stash has merge-symbols';
is GLOBAL::<Foo>.^name, 'Foo', 'a global lookup through the stash';
ok GLOBAL::.keys.elems > 0, 'the global stash has keys';
