use Test;
use lib 't/lib';

# An EVAL'd unit's top-level `use` runs before its CHECK-time validation, so
# what a `sub EXPORT` hook imports is in scope for it (#13441).
plan 7;

is EVAL(q[use EvalExportHookUpper; Thing]), 42, 'a capitalised term the hook returns';
is EVAL(q[use EvalExportHookUpper; shout("a")]), 'A', 'a routine the hook returns';

is EVAL(q[use EvalExportHookTerm <U>; U.k]), 1, 'terms named by the use arguments';
throws-like { EVAL q[use EvalExportHookTerm <U>; nosuch] }, X::Undeclared::Symbols,
    'a name the hook did not bring in is still undeclared at CHECK time';

# The import is lexical to the EVAL.
EVAL q[use EvalExportHookUpper; 1];
throws-like { EVAL q[Thing] }, X::Undeclared::Symbols, 'the import does not outlive the EVAL';

# Each EVAL runs the hook with its own `use` arguments.
is EVAL(q[use EvalExportHookTerm <A B>; B.k]), 2, 'second term';
is EVAL(q[use EvalExportHookTerm <A B C>; C.k]), 3, 'a different use argument list is its own import';
