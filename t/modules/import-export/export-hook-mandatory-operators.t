use Test;
use lib 't/lib-hook-mandatory-operators';

# The operators a `sub EXPORT` hook module binds into its EXPORT::MANDATORY
# stash at run time are known to the importer's parse (#18091, the annotations
# distribution): the module is loaded (or, when its hook may bind
# `LEXICAL::<EXPORTHOW>`, activated like a slang) while the importer parses,
# and what it exports reaches the parser. The activation reported no routines.

plan 4;

{
    use WordsHook 'x';
    is (1 whk 2), 'whk(1; 2)', 'a MANDATORY operator parses';
    is (1 whk 2, 3), 'whk(1; 2,3)', 'with its declared precedence';
}

{
    use WordsDecl <x y>;
    is (1 wdc 2, 3), 'wdc(1; 2,3)', 'a hook that can bind EXPORTHOW: a MANDATORY operator parses';
}

is EVAL(q[use WordsDecl 'x'; 4 wdc 5]), 'wdc(4; 5)', 'in an EVAL, from its top level';
