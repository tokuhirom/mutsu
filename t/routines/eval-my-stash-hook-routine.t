use Test;
use experimental :rakuast;

plan 6;

# A routine a `sub EXPORT` hook handed out is in the `MY::` pad of the EVAL
# that `use`d the module, for a string EVAL and for an EVAL of a tree.

my $code = Q|use lib 't/lib'; use CustomExportLexicalStashFixture;
    (MY::<&exported-by-lexical-stash>:exists, exported-by-lexical-stash(), MY::.keys.grep(/export/).sort.List)|;

my @string = EVAL $code;
is @string[0], True, 'string EVAL: MY:: holds the hook routine';
is @string[1], 'from custom EXPORT', 'and the routine is callable';
is-deeply @string[2], ('&exported-by-lexical-stash',), 'and MY::.keys lists it';

my @tree = EVAL $code.AST;
is @tree[0], True, 'tree EVAL: MY:: holds the hook routine';
is @tree[1], 'from custom EXPORT', 'and the routine is callable';
is-deeply @tree[2], ('&exported-by-lexical-stash',), 'and MY::.keys lists it';
