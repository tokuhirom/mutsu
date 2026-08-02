use Test;

# `.cache` is what makes a Seq *repeatable* in rakudo: after it, the Seq serves
# its cached elements instead of throwing "The iterator of this Seq is already
# in use/consumed by another Seq". (Only `.cache` -- `.list`/`.List` consume
# like any other method.) mutsu's lazy IO-lines value only recorded "consumed",
# so forcing it and dropping the result left the receiver variable holding a
# spent value, and the very next method call on it died -- which is what
# `Test::Util`'s `is-eqv` does (`$got.cache; $expected.cache;` and only then
# compares), and why `roast/S16-io/words.t`'s `$*ARGFILES` assertion failed
# under the real module.

plan 5;

my $file = $*TMPDIR.child("mutsu-seq-cache-{$*PID}.txt");
$file.spurt: "foo bar\nmeow moo\n";
LEAVE try $file.unlink;

my $words = $file.words;
$words.cache;
is-deeply $words.List, <foo bar meow moo>.List, '.cache makes a words Seq repeatable';
is-deeply $words.List, <foo bar meow moo>.List, '... and it stays repeatable';
is-deeply $words.elems, 4, '... and the cached Seq still answers .elems';

my $lines = $file.lines;
$lines.cache;
is-deeply $lines.List, ('foo bar', 'meow moo').List, '.cache does the same for lines';

# The shape `is-eqv` uses: cache, then compare.
my $again = $file.words;
$again.cache;
ok $again eqv <foo bar meow moo>.Seq, 'a cached Seq compares eqv';
