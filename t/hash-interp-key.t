use v6;
use Test;

# A `{ ... }` whose first element is an interpolated-string hash key
# (`{ "$_" => v }`, `{ "{ $x }B" => v }`) used to fail to parse: the hash-literal
# body parser only accepted compile-time literal/bareword keys, so an
# interpolated key was neither a valid hash key nor left the block as a closure.
#
# rakudo's rule: `{ ... }` is a hash composer UNLESS the body references the
# topic `$_` (directly, or via a `.^`/`.?`/`.method` on the implicit topic), in
# which case it is a block. So a topic-free interpolated key is a hash; a
# topic-referencing one is a block. (Real dist: Configuration::Utils writes
# `.duckmap({ "{ .^name }Builder" => generate-builder-class $_ })`.)

plan 9;

# Topic-free interpolated key → hash composer.
my $v = 3;
my %h = { "{ $v }B" => 1 };
is %h<3B>, 1, 'topic-free interpolated key builds a hash';

my %m = { a => 1, "{ $v }x" => 2 };
is %m<a>, 1, 'literal key alongside an interpolated key (literal part)';
is %m<3x>, 2, 'literal key alongside an interpolated key (interpolated part)';

# Topic-referencing interpolated key → block (closure), one pair per element.
my @a = (1, 2).map({ "$_" => 10 });
is @a.map(*.key).join(","), '1,2', 'topic $_ in an interpolated key forces a block';
is @a.map(*.value).join(","), '10,10', 'topic-key block yields one pair per element';

my @b = (1, 2).map({ "{ $_ }B" => $_ * 10 });
is @b.map({ .key ~ '=' ~ .value }).join(","), '1B=10,2B=20',
    'interpolated-topic key with a topic value is a block';

# `.^`/`.method` on the implicit topic also forces a block.
my @c = (1, 2).map({ "pre{ .Str }" => $_ });
is @c.map(*.key).join(","), 'pre1,pre2', 'implicit-topic .method in the key forces a block';

# A leading `.5` decimal value is not mistaken for a topic method call.
my %d = { a => .5 };
is %d<a>, 0.5, 'a decimal value after => keeps the hash a hash';

# Plain literal-key hash still composes a Hash (not a block) even in map sink.
my $composed = { "x" => 10 };
isa-ok $composed, Hash, 'a literal-key { } is still a hash composer';
