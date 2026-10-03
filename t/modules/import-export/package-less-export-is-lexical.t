use v6;
use lib 't/lib';
use Test;
use MONKEY-SEE-NO-EVAL;

# A package-less module's `sub f is export` is lexical to the module, exactly
# like its unexported subs: `need` alone does not expose it, and a
# block-scoped `use` exposes it only inside the block (#11103).

plan 6;

{
    need PlainFileOwnExport;
    throws-like { EVAL 'plain-own-export()' }, X::Undeclared::Symbols,
        '`need` alone does not make an exported `my` sub callable';
}
{
    use PlainFileOwnExport;
    is plain-own-export(), 'own:helper', 'the import makes it callable in the block';
    is plain-own-caller(), 'own:helper',
        'the module\'s own subs still reach it by bare name';
}
throws-like { EVAL 'plain-own-export()' }, X::Undeclared::Symbols,
    'the block-scoped import does not outlive the block';
throws-like { EVAL 'plain-own-helper()' }, X::Undeclared::Symbols,
    'an unexported helper stays private';
is EVAL('use PlainFileOwnExport; plain-own-export()'), 'own:helper',
    'a later import still finds the export';
