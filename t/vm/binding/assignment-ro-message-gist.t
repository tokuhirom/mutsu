use Test;

# rakudo's `X::Assignment::RO.message` names the value's type and shows its
# `.gist`, cut to 20 characters plus "..." when the gist is longer than 23.

plan 8;

is X::Assignment::RO.new(value => 'x' x 23).message,
    'Cannot modify an immutable Str (' ~ 'x' x 23 ~ ')', 'a 23-character gist is whole';
is X::Assignment::RO.new(value => 'x' x 24).message,
    'Cannot modify an immutable Str (' ~ 'x' x 20 ~ '...)', 'a longer one is cut';
is X::Assignment::RO.new(value => (1..30).List).message,
    'Cannot modify an immutable List ((1 2 3 4 5 6 7 8 9 1...)', 'a List names its type';
is X::Assignment::RO.new(value => (a => 1)).message,
    'Cannot modify an immutable Pair (a => 1)', 'a Pair shows its gist';

my $l = (1..30).List;
throws-like { $l[0] = 3 }, X::Assignment::RO,
    message => 'Cannot modify an immutable List ((1 2 3 4 5 6 7 8 9 1...)', 'a store into a List';

my %s := set <a>;
throws-like { %s = 1 }, X::Assignment::RO,
    message => 'Cannot modify an immutable Set (Set(a))', 'reassigning a Set-bound hash';
my %b := bag <a a>;
throws-like { %b = 1 }, X::Assignment::RO,
    message => 'Cannot modify an immutable Bag (Bag(a(2)))', 'reassigning a Bag-bound hash';
my %m := Map.new((a => 1));
throws-like { %m = 1 }, X::Assignment::RO,
    message => 'Cannot modify an immutable Map (Map.new((a => 1)))', 'reassigning a Map-bound hash';
