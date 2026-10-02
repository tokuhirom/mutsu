use v6;
use lib 't/lib';
use Test;
use MONKEY-SEE-NO-EVAL;

# An EVAL is its own lexical scope: what a `use` inside it imports is gone
# once the EVAL returns, so a later EVAL cannot see it (#11069).

plan 6;

is EVAL('use EvalExportHookTerm <U>; U.k'), 1,
    'a term a `sub EXPORT` hook returns is visible inside its EVAL';
throws-like { EVAL 'U' }, X::Undeclared::Symbols,
    'that term is not visible to a later EVAL';
throws-like { EVAL 'need EvalExportHookTerm; U' }, X::Undeclared::Symbols,
    'nor to a later EVAL that only `need`s the module';

is EVAL('use EvalScopedExportClass; EvalScopedThing.v'), 'thing',
    'an exported class is visible inside its EVAL';
# The class name itself still resolves afterwards: mutsu keeps a loaded
# module's own top-level declarations registered globally (#11103).

is EVAL('class EvalOwnClass { method v { 7 } }; 1') && EVAL('EvalOwnClass.v'), 7,
    'a class the EVAL itself declares still outlives it';

{
    my $shadowed = 'caller';
    EVAL q[use EvalExportHookTerm <$shadowed>; 1];
    is $shadowed, 'caller',
        'an import inside EVAL does not rebind the caller\'s same-named variable';
}
