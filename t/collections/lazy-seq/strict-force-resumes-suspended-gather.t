use v6;
use Test;

# A prior BOUNDED pull off a gather-based lazy list (an index read, or the
# refused element-store probe of #7556) leaves the gather coroutine suspended
# mid-body with the cache holding only a prefix. A subsequent STRICT/eager
# force (`.eager`, `.List`, `.Array`, ...) must resume that suspended
# coroutine to completion, not short-circuit on the partial cache and not
# restart the body from scratch, dropping every element already taken past
# the bounded pull's cursor (#8512).

plan 4;

{
    my $s = (gather { take 1; take 2 });
    is $s[0], 1, 'a bounded index pull leaves the coroutine suspended after one take';
    is-deeply $s.eager, (1, 2), 'a later .eager resumes the suspended coroutine instead of dropping take 2';
}

{
    my $s = (gather { take 1; take 2 });
    my $ = try { $s[0] = 5; };
    is $s[1], 2, 'the refused element-store probe also leaves the coroutine resumable';
    is-deeply $s.eager, (1, 2), '.eager after that probe still reaches every taken element';
}
