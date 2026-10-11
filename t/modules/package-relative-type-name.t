use Test;
use lib 't/lib';
use PkgRelType;

# From the Selkie::UI ecosystem distribution: inside `unit class Foo`, a bare
# `Bar` names the `Foo::Bar` that `use Foo::Bar` loaded (the stash of Foo).
plan 2;

is leaf(), "leaf", "bare short name resolves to Pkg::Leaf";
is list-elems(), 3, "a core name (List) is not shadowed by Pkg::List";
