use Test;

# From the Glob::Grammar distribution: `@hashes>>.<name glob regex>` slices
# every element's hash instead of looking the word list up as one key.
plan 4;

my @d = (%(a => 1, b => 2, c => 3), %(a => 4, b => 5, c => 6));
is-deeply (@d>>.<a b>).list, ((1, 2), (4, 5)), 'multi-word angle slice per element';
is-deeply (@d>>.<a>).list, (1, 4), 'single key';

my @seen;
for @d>>.<a b c> -> ($x, $y, $z) { @seen.push: "$x$y$z" }
is-deeply @seen, ["123", "456"], 'sub-signature unpack in for';

for @d>>.<a b> -> $pair { @seen.push: $pair.elems }
is-deeply @seen[2, 3], (2, 2), 'each slice is one item';
