use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# ADR-0058 step 3b made `.grep` produce a deferred Seq, so its callback now runs
# at the PULL, in whatever frame consumes the Seq -- not at the `.grep` call.
# The grep loop's capture merge was written for the old timing: it overwrote
# every name in the block's captured env unconditionally, while saving only the
# names the consuming frame did not already have. A name held by both was
# therefore clobbered with its capture-time value and never restored.
#
# The minimal victim is the variable the Seq is being assigned to:
#
#     my $s = (1, 2, 3, 4).grep({ $_ %% 2 });   # `s` is captured here, as Any
#     $s.elems;                                 # pull: `s` is restored to Any
#
# `.map` had the caller-priority merge already; `.grep` and `.first`'s batched
# matcher did not. Only observable once `reflective_name_access_possible()` has
# latched, which makes the env mirror authoritative -- so the EVAL below, and in
# real programs any file that loads the upstream `Test` module (its
# `throws-like` EVALs a string).

plan 8;

my $latch = EVAL '1';
is $latch, 1, 'EVAL latched the reflective-name-access flag';

{
    my $s = (1, 2, 3, 4).grep({ $_ %% 2 });
    is $s.elems, 2, 'a self-assigned grep Seq reifies';
    is $s.^name, 'Seq', '... and the variable still holds it afterwards';
    is $s.sum, 6, '... and it is still re-readable';
}

{
    my @a = 1, 2, 3, 4;
    my $s = @a.grep({ $_ %% 2 });
    is $s.elems, 2, 'the promoting array arm reifies';
    is $s.^name, 'Seq', '... and leaves its own variable alone';
}

# A captured free variable is a shared container, so the deferred callback sees
# the value it holds AT THE PULL -- and the caller's own lexical must come back
# unchanged afterwards. (rakudo agrees on both halves: 0 matches, `$n` still 99.)
{
    my $n = 1;
    my $s = (1, 2, 3).grep({ $_ > $n });
    $n = 99;
    is $s.elems, 0, 'the deferred callback reads its captured container at the pull';
    is $n, 99, '... and the caller lexical survives the pull';
}
