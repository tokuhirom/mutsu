use Test;

# From Config::TOML (t/exceptions/02-dumper.rakutest): a Match is a Capture,
# and a Capture is not Associative, so `Associative:D` multi candidates must
# not claim it.

plan 3;

my $m = 'hello' ~~ /hello/;
nok $m ~~ Associative, 'Match is not Associative';

multi sub f(Associative:D $) { 'assoc' }
multi sub f($) { 'other' }
is f($m), 'other', 'Match skips the Associative:D candidate';
is f(%(a => 1)), 'assoc', 'Hash still takes the Associative:D candidate';
