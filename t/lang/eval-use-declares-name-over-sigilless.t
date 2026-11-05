use Test;

# #15806: the `use X` of an EVAL'd unit declares `X` in that unit, so the bareword
# `X` is the imported package, not a sigilless `\X` visible at the EVAL call site.

plan 6;

use lib 't/lib';

{
    my \EvalUseDeclaredDep = 1;
    is "use EvalUseDeclaredDep; EvalUseDeclaredDep".EVAL.^name, 'EvalUseDeclaredDep',
        'an enclosing sigilless of the same name does not shadow the EVAL unit\'s import';
}

my \Other = try "use EvalUseDeclaredDep; EvalUseDeclaredDep".EVAL;
is Other.^name, 'EvalUseDeclaredDep', 'bound to a differently named sigilless';

sub load { "use EvalUseDeclaredDep; EvalUseDeclaredDep".EVAL }
my \EvalUseDeclaredDep = try "use EvalUseDeclaredDep; EvalUseDeclaredDep".EVAL;
is EvalUseDeclaredDep.^name, 'EvalUseDeclaredDep', 'the self-referential form (Humming-Bird)';
is load().^name, 'EvalUseDeclaredDep', 'an EVAL inside a routine called from that scope';

{
    my \EvalUseDeclaredDep = 5;
    is load().^name, 'EvalUseDeclaredDep', 'the routine\'s EVAL ignores the caller\'s sigilless';
    is "use EvalUseDeclaredDep; EvalUseDeclaredDep.hi".EVAL, 'hi from Dep', 'and the package is usable';
}
