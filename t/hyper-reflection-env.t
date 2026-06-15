# Pin test for CP-2 shrink slice 1: dropping spurious `env_dirty` marks on the
# pure hyper/race wrap + reflection (is-lazy/^name/WHAT/isa/does/defined)
# method branches in vm_call_method_ops.rs. These ops are env-pure, so they must
# not force an env->locals pull. Interleave them with local mutations/reads to
# confirm no stale read is introduced (and no value is lost).
use Test;

plan 10;

# Pure hyper/race wrap interleaved with a local read.
{
    my $x = 7;
    my @r = (1, 2, 3).hyper.map(* + $x);   # block reads captured $x = 7
    is @r.sort.join(","), "8,9,10", "hyper.map reads captured local correctly";
    is $x, 7, "local intact after hyper.map";
}

# Reflection methods on hyper/race, then read a freshly-mutated local.
{
    my $n = 1;
    my $h = (1, 2, 3).hyper;
    is $h.WHAT.^name, "HyperSeq", "hyper.WHAT then ...";
    $n = $n + 40;
    is $n, 41, "local mutation observed after hyper.WHAT";
    is $h.is-lazy, False, "hyper.is-lazy is False";
    is $h.defined, True, "hyper.defined is True";
    is $h.^name, "HyperSeq", "hyper.^name";
}

# race wrap/rewrap and a subsequent local read
{
    my $acc = 0;
    my @out = (1, 2, 3, 4).race.grep(* %% 2);
    $acc = @out.elems;
    is @out.sort.join(","), "2,4", "race.grep result";
    is $acc, 2, "local assigned from race result";
}

# isa/does on a hyper value
{
    my $h = (1, 2).hyper;
    ok $h.isa(Any), "hyper isa Any";
}
