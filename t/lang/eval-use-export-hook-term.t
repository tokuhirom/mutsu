use v6;
use lib 't/lib';
use Test;
use MONKEY-SEE-NO-EVAL;

# A module whose `sub EXPORT` computes the exported term names from the `use`
# arguments: EVAL's pre-run "Undeclared name" check cannot list them, and must
# not reject them (#11062, Logic::Ternary t/04-export.rakutest).

plan 4;

# Runs first: an earlier EVAL's imports must not be in scope here.
throws-like { EVAL 'need EvalExportHookTerm; U' },
    X::Undeclared::Symbols,
    '`need` runs no export hook, so its terms are not declared';

is EVAL('use EvalExportHookTerm <U>; U.k'), 1,
    'a term exported by a dynamic `sub EXPORT` is declared inside EVAL';
is EVAL('use EvalExportHookTerm <P Q>; Q.k'), 2,
    'the names follow the `use` arguments';

throws-like { EVAL 'use EvalExportHookTerm <U>; NoSuchTerm' },
    X::Undeclared::Symbols,
    'an undeclared name after such a `use` is still rejected';
