use Test;

# A runtime helper that rebuilds a whole container from scratch must write the
# rebuilt contents THROUGH the existing backing node, not drop a fresh node into
# `env` under the variable's bare name. A replacement is invisible to every
# other holder of that container -- a `:=`-bound alias, a by-value capture, and
# (once `@`/`%` reads are slot-addressed, ADR-0039 slice 2) the declaring
# frame's own local slot.
#
# Every assertion is byte-identical under `raku`.

plan 10;

# --- the listop `map` rw element writeback ----------------------------------

{
    my @a = 1, 2, 3;
    my $alias := @a;
    map { $_ = 5 }, @a;
    is-deeply @a, [5, 5, 5], 'listop map rw writeback updates the source';
    is-deeply $alias, [5, 5, 5], 'and the := alias observes it';
}

{
    my @a = 1, 2, 3;
    my @holder = (0, @a);
    map { $_ = 7 }, @a;
    is-deeply @a, [7, 7, 7], 'listop map rw writeback updates the source (capture)';
    is-deeply @holder[1], [7, 7, 7], 'and a by-value capture of the container observes it';
}

# --- the `.map` method rw element writeback ---------------------------------

{
    my @b = 1, 2, 3;
    my $alias := @b;
    @b.map({ $_ = 8 }).eager;
    is-deeply @b, [8, 8, 8], '.map rw writeback updates the source';
    is-deeply $alias, [8, 8, 8], 'and the := alias observes it';
}

# --- `classify` / `categorize` writing into a caller's hash -----------------

{
    my %into;
    my $alias := %into;
    my @src = (1, 2, 3, 4);
    @src.categorize({ $_ %% 2 }, into => %into);
    is %into.keys.sort.join(","), 'False,True', 'categorize :into fills the target';
    is $alias.keys.sort.join(","), 'False,True', 'and the := alias observes it';
}

{
    my %into;
    my $alias := %into;
    my @src = <a bb ccc>;
    @src.classify({ .chars }, into => %into);
    is %into.keys.sort.join(","), '1,2,3', 'classify :into fills the target';
    is $alias.keys.sort.join(","), '1,2,3', 'and the := alias observes it';
}
