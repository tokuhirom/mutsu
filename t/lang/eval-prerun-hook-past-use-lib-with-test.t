use Test;
use MONKEY-SEE-NO-EVAL;

# An EVAL'd unit that `use`s Test and a `sub EXPORT` hook module after a
# literal `use lib` sees the hook's names at CHECK time (#13441, #16695), and
# the hook's exported `&trait_mod:<is>` dispatcher keeps its candidate next to
# Test's. Checked against rakudo (run with -I t/lib).

plan 4;

is EVAL(Q[use Test; use lib "t/lib"; use EvalExportHookUpper; Thing]), 42,
    'a hook name after use Test + use lib is known at CHECK time';
is EVAL(Q[use lib "t/lib"; use Test; use EvalExportHookUpper; Thing]), 42,
    'and with use Test after use lib';

my $count = q[&trait_mod:<is>.candidates.map(*.signature.raku).grep(/xdtrait/).elems];
is EVAL("use Test; use lib 't/lib'; use ExportSubTraitDispatcher; $count"), 1,
    'the hook dispatcher keeps its candidate next to Test\'s (use lib first)';
is EVAL("use lib 't/lib'; use Test; use ExportSubTraitDispatcher; $count"), 1,
    'and with use Test after use lib';
