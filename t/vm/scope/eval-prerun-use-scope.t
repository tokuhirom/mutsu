use lib 't/lib';
use Test;

# An EVAL'd unit runs its top-level `use` and `import` statements before its
# CHECK-time validation, as rakudo does at BEGIN time, and the import stays
# lexical to the EVAL: the unit's own import wins over the caller's, and the
# caller keeps its own afterwards. Expected values are the `raku` output.

plan 5;

use EvalPreRunA;

is who(), 'A', 'the caller imports who from EvalPreRunA';
is EVAL(q[use EvalPreRunB; who()]), 'B', "the EVAL's own use wins over the caller's import";
is who(), 'A', "and the caller's import is intact afterwards";
is EVAL(q[use EvalPreRunA :extra; import EvalPreRunA :extra; only-a()]), 'only-a',
    'a use followed by an import of the same module';
throws-like q[use EvalPreRunB; no-such-routine()], X::Undeclared::Symbols,
    'the names of a pre-run use are known to the checks, others still fail';
