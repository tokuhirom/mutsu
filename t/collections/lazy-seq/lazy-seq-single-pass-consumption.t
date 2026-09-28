use Test;

# #9789: `.lazy` on an already-finite source (`(1..5).lazy`, `(1,2,3).lazy`,
# the `lazy 1,2,3` prefix) is stored as a `LazyList` whose cache is
# pre-filled rather than as a `Value::Seq`, so it fell outside the
# `X::Seq::Consumed` single-pass tracking every other Seq shape (`.map`,
# `.grep`, a deferred `IO::Handle.lines`, ...) already honours: a second
# `.eager` silently re-answered instead of throwing. `.map`'s finite Seq
# already threw correctly (raku's own behavior) -- this file pins the
# `.lazy` shape matching it, plus the two shapes that must stay exempt:
# an explicit `.cache` (adding `.cache` is literally the fix the
# X::Seq::Consumed message suggests) and a lazy Seq assigned into an `@`
# array (that array's own backing store, not Seq semantics).

plan *;

for 'eager', 'sink' -> $method {
    my $s = (1..5).lazy;
    $s."$method"();
    throws-like { $s.eager }, X::Seq::Consumed,
        "(1..5).lazy: a second .eager throws after a first .$method touch";
}

{
    my $s = (1, 2, 3).lazy;
    is $s.eager, (1, 2, 3), '(1,2,3).lazy: first .eager reads the elements';
    throws-like { $s.eager }, X::Seq::Consumed,
        '(1,2,3).lazy: a second .eager throws X::Seq::Consumed';
}

{
    my $s = lazy 1, 2, 3;
    is $s.eager, (1, 2, 3), 'lazy 1,2,3 prefix: first .eager reads the elements';
    throws-like { $s.eager }, X::Seq::Consumed,
        'lazy 1,2,3 prefix: a second .eager throws X::Seq::Consumed';
}

# `.cache` is the documented fix for X::Seq::Consumed -- it must actually
# grant multi-pass reads, not merely return a value that consumes on its
# own first touch.
{
    my $s = (1..5).lazy.cache;
    is $s.eager, (1, 2, 3, 4, 5), '.lazy.cache: first .eager reads the elements';
    is $s.eager, (1, 2, 3, 4, 5),
        '.lazy.cache: a second .eager reads them again instead of throwing';
}

# A lazy Seq assigned into an `@` array is that array's own backing store
# (Array semantics), not single-pass Seq semantics.
{
    my @a = (1..5).lazy;
    is @a.eager, [1, 2, 3, 4, 5], '@a = (1..5).lazy: first .eager reads the elements';
    is @a.eager, [1, 2, 3, 4, 5],
        '@a = (1..5).lazy: a second .eager reads them again instead of throwing';
}

done-testing;
