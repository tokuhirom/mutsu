use Test;

plan 3;

# Issue #9838: a hyper method call (`».`) over a finite lazy Seq answered `()`
# instead of reifying it. The forcing guard confused "`.is-lazy` True"
# (`is_genuinely_lazy`) with "unsafe to force" -- an explicitly `lazy`-marked
# but finite gather is `.is-lazy` True yet perfectly safe to reify, so it must
# still be forced. Only a genuinely infinite/unreifiable list is left alone.
is((lazy gather { take 1 })».succ, (2,), 'hyper over an explicitly lazy-marked finite gather reifies');

# `indir` hands the block's return value back unchanged; combined with the
# fix above, hypering the resulting Seq must not answer `()` either.
{
    my $h = indir "/tmp", { gather { take 3 } };
    is($h».succ, (4,), 'hyper over the Seq indir returns reifies');
}

# A genuinely infinite source must still not be forced eagerly by the guard
# (it stays unreified going into the fallback path rather than hanging).
lives-ok({ (1..*)».succ }, 'hyper over a genuinely infinite lazy source does not hang');
