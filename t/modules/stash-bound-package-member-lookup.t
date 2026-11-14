use Test;

# Packages a program builds at run time through the metaobject protocol and
# binds through stashes (`$pkg.WHO.BIND-KEY`), as Grammar::BNF's slang does:
#
# - a type `new_type` minted under a qualified name is installed nowhere until
#   something binds it, so a fresh stub package of its first part has an empty
#   stash;
# - a qualified name whose prefix a stash binding placed under another package
#   resolves through that package's stash.

plan 6;

my Mu $g := Metamodel::ClassHOW.new_type(:name('Deep::Down::G'));
$g.^add_method('hi', method { 'hi' });
$g.^compose;

my Mu $deep := Metamodel::PackageHOW.new_type(:name('Deep'));
nok $deep.WHO.EXISTS-KEY('Down'), "a minted type's name implies no package in a fresh stub";
is-deeply $deep.WHO.keys.List, (), 'the fresh stub stash is empty';

GLOBAL.WHO.BIND-KEY('Deep', $deep);
my Mu $down := Metamodel::PackageHOW.new_type(:name('Deep::Down'));
$deep.WHO.BIND-KEY('Down', $down);
$down.WHO.BIND-KEY('G', $g);
is ::('Deep::Down::G').hi, 'hi', 'the bound type resolves by its path';

module M { }
my Mu $nested := Metamodel::PackageHOW.new_type(:name('Nested'));
M.WHO.BIND-KEY('Nested', $nested);
my Mu $in := Metamodel::ClassHOW.new_type(:name('In'));
$in.^add_method('where', method { 'in' });
$in.^compose;
$nested.WHO.BIND-KEY('In', $in);
is M.WHO<Nested>.WHO<In>.where, 'in', 'through the stashes';
is ::('M::Nested::In').where, 'in', 'through the bound package';
is ::('M::Nested').^name, 'Nested', 'the bound package keeps its own name';

# vim: expandtab shiftwidth=4
