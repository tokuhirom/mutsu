use Test;

# `.[...]` is a subscript on the topic, so it takes a comma-separated *slice*
# list exactly as `$_[...]` and `$x.[...]` do. mutsu parsed the index with the
# single-expression parser, which stopped at the comma and left the `]` check
# to fail, so the whole term fell through to a bare "Confused." — every
# `.[0, 1]` in a file was a hard parse error.
#
# Found in the Test::Async distribution: `Test::Async::Base`'s `cmp-deeply`
# writes `self.expected-got(|.[0, 1], |.[2..*].Capture)`, which made the whole
# compunit unparsable.

plan 9;

$_ = [10, 20, 30, 40];

is .[0], 10, 'single-element topic subscript still works';
is-deeply .[0, 1], (10, 20), 'comma slice on the topic';
is-deeply .[1, 3], (20, 40), 'non-adjacent comma slice on the topic';
is-deeply .[0 .. 1], (10, 20), 'range slice on the topic still works';
is-deeply .[1 .. *], (20, 30, 40), 'open-ended range slice on the topic';

sub joined(*@a) { @a.join(',') }
is joined(|.[0, 1]), '10,20', 'flattened comma slice as call arguments';
is joined(|.[0, 1], |.[2 .. *]), '10,20,30,40', 'two flattened topic slices in one call';

# Inside a block, where `$_` is the block parameter rather than the outer topic.
is-deeply ([[1, 2, 3],].map({ .[0, 2] }).flat.List), (1, 3), 'comma slice on a block topic';

my %h = a => 1, b => 2, c => 3;
$_ = %h;
is-deeply .{'a', 'c'}, (1, 3), 'comma slice in a topic associative subscript';
