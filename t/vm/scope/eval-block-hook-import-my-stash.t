use lib 't/lib';
use Test;

plan 6;

# A `use` of a `sub EXPORT` hook module in a block of an EVAL'd unit binds in
# that block's pad (#14480).
is-deeply EVAL(q[{ use EvalHookSelective 'after'; MY::<&after>:exists }]), True,
    'MY:: lists a hook routine imported by a block of an EVAL';
is-deeply EVAL(q[{ use EvalHookSelective 'after'; MY::.keys.grep(/after/).List }]),
    ('&after',), 'MY::.keys names it';
is-deeply EVAL(q[sub { use EvalHookSelective 'after'; MY::<&after>:exists }()]), True,
    'in a routine body too';
is-deeply EVAL(q[{ use EvalHookSelective 'before'; MY::<&after>:exists }]), False,
    'only the imported names';

# The hook runs once per `use`, not once more for the EVAL's pre-run.
my $top = EVAL q[use EvalHookSelective 'after'; hook-runs()];
my $nested = EVAL q[my $c; { use EvalHookSelective 'after'; $c = hook-runs() }; $c];
is $nested - $top, 1, 'a nested use runs the hook once';
is EVAL(q[{ use EvalHookSelective 'after'; after("x") }]), 'ax', 'the routine is callable';
