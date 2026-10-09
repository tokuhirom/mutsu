use Test;

# From Text::Emoji: `my $x := BEGIN my %h is Map::Match = ...` applied the
# container trait twice, the second time STOREing the instance into itself.
plan 2;

class K { method STORE(*@v) { $*calls.push: @v.elems; self } }

my $*calls = [];
my $x = do my %h is K = (a => 1, b => 2);
is-deeply $*calls, [2], 'STORE runs once, with the two initializer pairs';

$*calls = [];
my $y = do { my %g is K = (c => 3); 5 };
is-deeply $*calls, [1], 'block form STOREs once too';
