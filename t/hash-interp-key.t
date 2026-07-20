use v6;
use Test;

# A `{ ... }` whose first element is an interpolated-string hash key
# (`{ "$_" => v }`, `{ "{ $x }B" => v }`) used to fail to parse: the hash-literal
# body parser only accepted compile-time literal/bareword keys, so an
# interpolated key was neither a valid hash key nor left the block as a closure.
#
# rakudo's rule: `{ ... }` is a hash composer UNLESS the body references the
# *explicit* topic `$_`/`@_`/`%_` at its top level (in the key or the value), in
# which case it is a block. A topic-free interpolated key is a hash; an implicit
# `.method`/`.^name` on the topic does NOT force a block. (Real dist:
# Configuration::Utils writes
# `.duckmap({ "{ .^name }Builder" => generate-builder-class $_ })` — a block
# because of the explicit `$_` in the value.)

plan 11;

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

# An interpolated key with an implicit-topic `.method`/`.^name` but NO explicit
# `$_` is still a HASH (rakudo does not treat an implicit-topic method as a
# block trigger). The explicit `$_` in the value here is what makes it a block.
my @c = (1, 2).map({ "pre{ .Str }" => $_ });
is @c.map(*.key).join(","), 'pre1,pre2', 'explicit $_ value with an implicit-topic key is a block';

isa-ok { "{ .^name }X" => 1 }, Hash,
    'an implicit-topic .^name in the key alone stays a hash';

# A leading `.5` decimal value is not mistaken for a topic method call.
my %d = { a => .5 };
is %d<a>, 0.5, 'a decimal value after => keeps the hash a hash';

# Plain literal-key hash still composes a Hash (not a block) even in map sink.
my $composed = { "x" => 10 };
isa-ok $composed, Hash, 'a literal-key { } is still a hash composer';

# A `$_` inside a *nested* block belongs to that inner block, so the outer
# braces stay a hash composer (regression guard for S32-list/map.t:250).
is {foo => (1, 2, 3).map: { $_ }}<foo>.join(":"), '1:2:3',
    'topic in a nested block does not force the outer braces to a block';
