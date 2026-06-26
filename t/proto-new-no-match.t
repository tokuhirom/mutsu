use Test;

plan 7;

# An explicit `proto method new` overrides the inherited Mu.new proto, so it
# owns dispatch entirely. A call that matches no candidate must surface as
# X::Multi::NoMatch rather than falling back to the default constructor (which
# would wrongly give X::Constructor::Positional).
throws-like 'class Polar {
    proto method new(|) { * }
    multi method new(Real \mag, Real \theta) { }
}
Polar.new(0e0)', X::Multi::NoMatch,
    'proto+multi new with no matching candidate throws X::Multi::NoMatch';

# A matching candidate still dispatches normally.
{
    my class P {
        proto method new(|) { * }
        multi method new(Int \a, Int \b) { self.bless }
    }
    my $p = P.new(1, 2);
    ok $p.defined, 'matching proto+multi new candidate still constructs';
}

# Without an explicit proto, an unmatched `multi method new` falls back to the
# inherited default constructor (Mu.new), which rejects positional args.
throws-like 'class Q {
    multi method new(Real \mag, Real \theta) { }
}
Q.new(0e0)', X::Constructor::Positional,
    'no-proto multi new still falls back to default constructor';

# A `constant` initializer is evaluated at BEGIN time, so an uncaught exception
# while evaluating it surfaces as X::Comp::BeginTime with the original nested.
throws-like 'class Polar {
    proto method new(|) { * }
    multi method new(Real \mag, Real \theta) { }
}
constant j = Polar.new(0e0)', X::Comp::BeginTime, exception => X::Multi::NoMatch,
    'constant init exception wraps in X::Comp::BeginTime (nested X::Multi::NoMatch)';

throws-like 'constant boom = die "kaboom"', X::Comp::BeginTime,
    'constant init die wraps in X::Comp::BeginTime';

# A successful constant initializer is unaffected.
{
    constant FORTY_TWO = 6 * 7;
    is FORTY_TWO, 42, 'normal constant initializer still works';
}
{
    constant @list = 1, 2, 3;
    is @list.elems, 3, 'constant list initializer still works';
}
