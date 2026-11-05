use Test;

plan 5;

# A `use lib` ahead of a `sub EXPORT` hook module's `use` in an EVAL'd unit:
# the unit's CHECK-time name check has to see what the hook exports, so the
# hook's `use` runs ahead of the check with the `use lib` path in place.
is (EVAL q[use lib 't/lib'; use EvalExportHookUpper; Thing]), 42,
    'a hook-exported term after a use lib';
is (EVAL q[use lib 't/lib'; use EvalExportHookUpper; shout("a")]), 'A',
    'a hook-exported routine after a use lib';
is (EVAL q[use lib 't/lib'; BEGIN { }; use EvalExportHookUpper; Thing]), 42,
    'a BEGIN block between the use lib and the hook use';
throws-like { EVAL q[use lib 't/lib'; use EvalExportHookUpper; nosuch] },
    X::Undeclared::Symbols, 'an unknown name is still rejected at CHECK time';
is (EVAL q[use lib 't/lib', 't/lib'; use EvalExportHookUpper; Thing]), 42,
    'a use lib with several specs';
