use Test;

# A successful regex Match answers Bool without forcing its lazy capture map.
# Grammar cursors with an explicit Bool method still use that user override.
plan 4;

my $match = 'a' ~~ /a/;
ok $match.Bool, 'a successful Match is truthy';
ok ?$match, 'boolean context keeps a successful Match truthy';
nok 'b' ~~ /a/, 'a failed regex match is falsy';

grammar FalseCursor {
    method Bool { False }
    token TOP { 'a' }
}
nok FalseCursor.parse('a'), 'a grammar cursor still honors a user Bool method';
