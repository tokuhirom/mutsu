use Test;

# A hyphen without surrounding whitespace can start a kebab-case identifier.
# `now-$T0` is therefore not the subtraction `now - $T0`; Rakudo rejects the
# ambiguous spelling as an undeclared `now` routine. The rejection must be the
# same in an interpolation block and in ordinary expression position.

plan 3;

throws-like {
    EVAL q[my $T0 = now; "A { ((now-$T0)*1000).round(0.01) }"]
}, X::Undeclared::Symbols,
    'an unspaced hyphen in an interpolation block does not become subtraction';

throws-like {
    EVAL q[my $T0 = now; ((now-$T0)*1000).round(0.01)]
}, X::Undeclared::Symbols,
    'the same unspaced hyphen is rejected outside interpolation';

my $elapsed = EVAL q[my $T0 = now; ((now - $T0)*1000).round(0.01)];
ok $elapsed >= 0, 'whitespace still selects the subtraction operator';
