use Test;
use lib 't/lib';

# A `use` of a `sub EXPORT` hook module nested in a block of an EVAL'd unit
# runs before the unit's CHECK-time validation, like a top-level one (#13441).
plan 6;

is EVAL(q[{ use EvalExportHookUpper; Thing }]), 42, 'a capitalised term, in a bare block';
is EVAL(q[if True { use EvalExportHookUpper; Thing }]), 42, 'in a branch body';
is EVAL(q[{ use EvalExportHookUpper; shout("a") }]), 'A', 'a routine the hook returns';
is EVAL(q[{ use EvalExportHookTerm <U>; U.k }]), 1, 'terms named by the use arguments';
throws-like { EVAL q[{ use EvalExportHookTerm <U>; nosuch }] }, X::Undeclared::Symbols,
    'a lowercase name the hook did not bring in is undeclared at CHECK time';
throws-like { EVAL q[{ use EvalExportHookUpper; nosuchfn }] }, X::Undeclared::Symbols,
    'so is a call to an unknown routine';
