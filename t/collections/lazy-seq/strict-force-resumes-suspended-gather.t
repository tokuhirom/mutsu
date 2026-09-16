use v6;
use Test;

# A prior BOUNDED pull off a gather-based lazy list (e.g. an index read) can
# leave the gather coroutine suspended mid-body with the cache holding only a
# prefix. A subsequent STRICT/eager force (`.eager`, `.Array`, ...) must
# resume that suspended coroutine to completion, not short-circuit on the
# partial cache and not restart the body from scratch, dropping every
# element already taken past the bounded pull's cursor (#8512).

plan 4;

{
    my $s = (gather { take 1; take 2 });
    is $s[0], 1, 'a bounded index pull leaves the coroutine suspended after one take';
    is-deeply $s.eager, (1, 2),
        '.eager resumes the suspended coroutine instead of dropping take 2';
}

{
    # `.eager` on a bare LazyList is dispatched separately from `.Array`/
    # `.List` (`dispatch_eager_method`, not the generic VM force-and-coerce
    # path) -- exercise a second, non-adjacent take to confirm the resumed
    # body isn't just replaying from `ip = 0`.
    my $s = (gather { take 1; take 2; take 3; });
    is $s[0], 1, 'bounded pull of the first element only';
    is-deeply $s.Array, [1, 2, 3],
        '.Array after that pull still reaches every taken element';
}
