use v6;
use Test;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;

plan 4;

# A type a `use`d module declares, a stub `role R {...}` included, is the
# using unit's from the `use` on: the unit's own later `role R` completes it,
# and a use of `R` between the two is not an `Illegally post-declared type`
# (BTree: `use BTree::Renderer` stubs `role BTree {...}`, the module then uses
# `BTree` and declares the role further down).

lives-ok { EVAL 'use PostDeclStubUser; 1' }, 'a module that uses a stubbed role before completing it loads';

# A unit's own post-declaration is still the compile error.
throws-like { EVAL 'say Own1; class Own1 {}' }, X::Undeclared::Symbols,
    'a use before the unit\'s own class declaration is still rejected';
throws-like { EVAL '{ say Own2; class Own2 {} }' }, X::Undeclared::Symbols,
    'also inside a nested block';
lives-ok { EVAL 'use PostDeclStub::Source; role PostDeclTree2 { }; PostDeclTree2' },
    'a unit that uses a module and declares an unrelated role is unaffected';
