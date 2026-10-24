use v6;
use Test;

# Only a call-site `Pair` is a named argument (ADR-0021): a Pair held in a
# variable is the SUBJECT of `Rakudo::Internals::JSON.to-json`, not an option
# (mutsu#14870, Pakku::Command::Config's `to-json( $option, :!pretty )`).
plan 5;

my $o = :async;
is Rakudo::Internals::JSON.to-json($o, :!pretty), '{"async":true}', 'a held Pair is the subject';
is Rakudo::Internals::JSON.to-json($o), '{"async":true}', 'and with no options';
sub wrap(\obj, :$pretty = True) { Rakudo::Internals::JSON.to-json: obj, :$pretty, :sorted-keys }
is wrap($o, :!pretty), '{"async":true}', 'forwarded through a raw parameter';
is Rakudo::Internals::JSON.to-json((1, 2), :!pretty), '[1,2]', 'a literal named adverb is still an option';
is Rakudo::Internals::JSON.to-json({b => 1, a => 2}, :!pretty, :sorted-keys), '{"a":2,"b":1}', 'sorted-keys is still an option';
