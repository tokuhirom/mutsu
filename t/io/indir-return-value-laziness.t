use Test;

plan 3;

# Issue #9838: `indir` runs its block and must hand back its return value
# unchanged, but `call_sub_value` (the path `indir` calls through) used to
# unconditionally re-stamp EVERY returned LazyList with the
# `__mutsu_preserve_lazy_on_array_assign` marker -- the same marker
# `.is-lazy` reads. That turned a plain (`.is-lazy` False) `gather` into a
# falsely `.is-lazy` True one merely for having been returned via `indir`.
{
    my $h = indir "/tmp", { gather { take 3 } };
    is($h.is-lazy, False, 'indir does not mark an ordinary gather Seq as lazy');
    is($h».succ, (4,), 'the Seq indir returns still hypers correctly');
}

# An explicitly `lazy`-marked value returned through `indir` must keep its
# own laziness (the marker travels with the cloned Value already; it must
# not depend on any re-stamping in the call path).
{
    my $h = indir "/tmp", { lazy gather { take 5 } };
    is($h.is-lazy, True, 'indir preserves an explicitly lazy-marked Seq as lazy');
}
