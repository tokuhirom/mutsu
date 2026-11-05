use v6;
use lib 't/lib';
use Test;

# Spawning a thread (`start`, `Thread.start`) flattens the scoped env, which
# folds an open import region into the flat tier. An EVAL that `use`s a module
# and then starts a thread must still end cleanly (a debug build used to trip an
# assertion in `close_import_region`), and its imports must stay confined to the
# EVAL (#15870).

plan 5;

is EVAL(q[use ImportedPredicate; await start { 1 }; has-interp('a#{b}')]), True,
    'start inside an EVAL that uses a module';

is EVAL(q[use ImportedPredicate; Thread.start({ 1 }).finish; has-interp('plain')]), False,
    'Thread.start inside an EVAL that uses a module';

is EVAL(q[use ImportedPredicate; my $p = start { has-interp('x#{y}') }; await $p]), True,
    'the imported routine is visible in the started block';

nok (try EVAL('has-interp("x")')).defined,
    'the import did not leak out of the first EVAL';

{
    use experimental :rakuast;
    is Q[use ImportedPredicate; await start { 2 }; has-interp('a#{b}')].AST.EVAL, True,
        'the same through EVAL($ast)';
}
