use v6;
use Test;

# The DYNAMIC:: pseudo-stash (`DYNAMIC::<$*x>`) does a dynamic-scope search: it
# finds a `$*x` bound in ANY caller frame, whether assigned (`= v`, has a Scalar
# container) or `:=`-bound to a bare value with no container (roast
# 6.c/S02-names/pseudo-6c.t "Bound dynamics", test 161).

plan 5;

# assigned dynamic (has a container)
{
    my sub inner {
        ok DYNAMIC::<$*A>:exists, 'DYNAMIC:: sees an assigned dynamic';
        is DYNAMIC::<$*A>, 5, 'DYNAMIC:: reads an assigned dynamic value';
    }
    my sub outer { my $*A = 5; inner }
    outer;
}

# container-less bound dynamic (`:=` to a bare value)
{
    my sub inner {
        ok DYNAMIC::<$*BOUND>:exists, 'DYNAMIC:: sees a bound (no-container) dynamic';
        is DYNAMIC::<$*BOUND>, pi, 'bound dynamic value is accessible';
    }
    my sub outer { my $*BOUND := pi; inner }
    outer;
}

# an unfound dynamic is absent, not an error
{
    my sub inner {
        nok DYNAMIC::<$*NOPE>:exists, 'DYNAMIC:: does not invent an absent dynamic';
    }
    inner;
}
