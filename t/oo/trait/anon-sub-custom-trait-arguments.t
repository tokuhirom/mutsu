use Test;

# An anonymous sub's custom `is` trait must receive the full parenthesized
# argument list and decorate the closure that was just created. This is the
# shape used by Sub::Memoized's `is memoized(%cache, &keyer)` trait.
plan 6;

my @trait-args;
multi sub trait_mod:<is>(Sub:D \routine, List:D :$memoized!) is export {
    @trait-args.push($memoized.elems);
    my \cache = $memoized[0]<>;
    my &keyer = $memoized[1];
    routine.wrap(-> |capture {
        my $key = keyer(capture);
        cache.EXISTS-KEY($key)
            ?? cache.AT-KEY($key)
            !! cache.BIND-KEY($key, callsame)
    });
}

my %cache;
my $body-calls = 0;
my &double = sub ($value) is memoized(%cache, { .[0] }) {
    ++$body-calls;
    $value * 2
};

is @trait-args.elems, 1, 'the anonymous sub trait handler runs once';
is @trait-args[0], 2, 'all anonymous sub trait arguments reach the handler';
is double(3), 6, 'the custom trait preserves the anonymous sub result';
is double(3), 6, 'the custom trait wrapper serves the cached result';
is $body-calls, 1, 'the wrapped anonymous sub body runs once';
is-deeply %cache, { '3' => 6 }, 'BIND-KEY updates the caller-visible cache';
