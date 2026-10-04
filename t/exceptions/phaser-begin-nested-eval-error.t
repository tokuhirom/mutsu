use Test;

# An exception escaping a BEGIN/CHECK phaser is wrapped in X::Comp::BeginTime,
# but one raised by an EVAL that the phaser body runs reaches that body
# unwrapped, so it can still be caught there (roast S06-advanced/stub.t's
# `BEGIN throws-like 'wind()', X::StubCode`). Measured on rakudo 2026.09.

BEGIN plan 4;

BEGIN {
    try { EVAL q[die "inner"] };
    is $!.^name, 'X::AdHoc', 'an EVAL inside BEGIN throws its own exception';
}

sub wind { !!! }
BEGIN throws-like 'wind()', X::StubCode, 'a stub run by an EVAL inside BEGIN dies with X::StubCode';

throws-like q[BEGIN { die "outer" }], X::Comp::BeginTime,
    'an exception escaping the BEGIN itself is still wrapped';

throws-like q[BEGIN { EVAL q[die "deep"] }], X::Comp::BeginTime,
    message => /deep/, 'and wrapped once, around the original exception';
