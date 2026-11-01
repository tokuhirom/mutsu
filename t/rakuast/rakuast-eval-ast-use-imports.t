use v6;
use experimental :rakuast;
use lib 't/lib';
use Test;

# `EVAL($ast)` of a tree that holds a `use`: the routines the module exports
# are known to the CHECK-time undeclared-routine check, as they are for the
# same text run as a string EVAL (#15868).

plan 4;

my $block-use = Q[
    {
        use ImportedPredicate;
        sub direct($s) { has-interp($s) }
        direct('plain');
    }
];
is-deeply $block-use.AST.EVAL, $block-use.EVAL,
    'a routine imported by a use inside a block is not undeclared in the tree';
is $block-use.AST.EVAL, False, 'the imported routine runs';

my $top = Q[use lib 't/lib'; use ImportedPredicate; has-interp('a#{b}')];
is $top.AST.EVAL, True, 'a top-level use after a use lib';

my $hook = Q[use lib 't/lib'; use InnerExportUser; p1()];
is $hook.AST.EVAL, 'provided-by-X1', 'a module that imports through a sub EXPORT hook';
