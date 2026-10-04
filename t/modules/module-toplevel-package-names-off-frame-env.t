use v6;
use lib 't/lib';
use Test;

plan 8;

# A qualified `package`/`unit module` a loaded module's mainline declares used
# to be bound under its own name in the importer's env, and so in every frame
# env of the program (ADR-0084, #7817). It is no longer bound there; the
# package's kind record answers instead. What is pinned here is that every way
# of reaching such a package still answers as rakudo does.

use TopPkgNames::A;

is TopPkgNames::A::a(), 'ac', "a package block's routine is reachable by its qualified name";
is TopPkgNames::C::c(), 'c', "so is a transitively loaded unit module's";
is TopPkgNames::A.^name, 'TopPkgNames::A', 'the package resolves as a bareword';
is TopPkgNames::C.HOW.^name, 'Perl6::Metamodel::ModuleHOW', "a unit module keeps its kind";
is ::('TopPkgNames::C').^name, 'TopPkgNames::C', 'indirect lookup finds a unit module';
is ::('TopPkgNames::A').^name, 'TopPkgNames::A', '... and a package block';
is-deeply TopPkgNames::.keys.sort.list, <A C>.list, 'both are members of the enclosing stash';
nok (try ::('TopPkgNames::Nope')).defined, 'an undeclared sibling is still not found';
