use Test;

# The routine form of the subscript operators, `postcircumfix:<[ ]>(@a, ...)`
# and `postcircumfix:<{ }>(%h, ...)`, classifies an adverb that is not a
# built-in subscript adverb the way the syntax form `@a[0,1]:foo` does (#10345):
# a slice candidate or an associative one slurps `*%_` and raises X::Adverb,
# while a single positional element and a call with several indices have no
# candidate (X::Multi::NoMatch). Expectations are rakudo's.

plan 23;

my %h = a => 1, b => 2;
my @a = 1, 2, 3;

# --- an associative subscript: X::Adverb ---
throws-like { postcircumfix:<{ }>(%h, "a", :foo) }, X::Adverb,
    what => 'slice', source => '%h', unexpected => <foo>, nogo => (),
    'a hash key with an unknown adverb';
throws-like { postcircumfix:<{ }>({ a => 1 }, "a", :foo) }, X::Adverb,
    what => 'slice', source => '%', 'an anonymous hash is the bare sigil';
throws-like { postcircumfix:<{ }>(%h, ("a", "b"), :foo) }, X::Adverb,
    what => 'slice', 'a hash slice with an unknown adverb';
throws-like { postcircumfix:<{ }>(%h, :foo) }, X::Adverb,
    what => '{} slice', source => '%h', 'the hash zen slice';

# --- a positional slice: X::Adverb ---
throws-like { postcircumfix:<[ ]>(@a, (0, 1), :foo) }, X::Adverb,
    what => 'slice', source => '@a', unexpected => <foo>, nogo => (),
    message => "Unexpected adverb 'foo' passed to slice on '@a'.",
    'a positional slice with an unknown adverb';
throws-like { postcircumfix:<[ ]>([1, 2], (0, 1), :foo) }, X::Adverb,
    what => 'slice', 'an anonymous array slice';
throws-like { postcircumfix:<[ ]>(@a, *, :foo) }, X::Adverb,
    what => 'whatever slice', 'a whatever slice';
throws-like { postcircumfix:<[ ]>(@a, 0..1, :foo) }, X::Adverb,
    what => 'slice', 'a range slice';
throws-like { postcircumfix:<[ ]>(@a, :foo) }, X::Adverb,
    what => 'zen slice', source => '@a', 'the array zen slice';

# --- a single positional element: no candidate takes only an unknown adverb ---
throws-like { postcircumfix:<[ ]>(@a, 0, :foo) }, X::Multi::NoMatch,
    'a single element with an unknown adverb';
throws-like { postcircumfix:<[ ]>(@a, 0, :foo(3)) }, X::Multi::NoMatch,
    'a single element with a valued unknown adverb';

# --- a built-in adverb next to an unknown one ---
throws-like { postcircumfix:<[ ]>(@a, 0, :k, :foo) }, X::Adverb,
    what => 'element access', source => '@a', unexpected => <foo>, nogo => <k>,
    'a single element with :k and an unknown adverb';
throws-like { postcircumfix:<{ }>(%h, "a", :exists, :foo) }, X::Adverb,
    what => 'slice', nogo => <exists>, 'a hash key with :exists and an unknown adverb';
throws-like { postcircumfix:<[ ]>(@a, 0, :delete, :foo) }, X::Adverb,
    nogo => <delete>, ':delete with an unknown adverb';
is-deeply @a, [1, 2, 3], '... and nothing was deleted';

# --- several unknown adverbs are counted and sorted ---
throws-like { postcircumfix:<{ }>(%h, "a", :foo, :bar) }, X::Adverb,
    unexpected => <bar foo>,
    message => "2 unexpected adverbs ('bar', 'foo') passed to slice on '%h'.",
    'several unknown adverbs';

# --- more than one index has no candidate at all ---
throws-like { postcircumfix:<{ }>(%h, "a", "b", :foo) }, X::Multi::NoMatch,
    'two indices with an unknown adverb';
throws-like { postcircumfix:<[ ]>(@a, 0, 1, :foo) }, X::Multi::NoMatch,
    'two positional indices with an unknown adverb';

# --- the recognized single-adverb calls are unchanged ---
is postcircumfix:<[ ]>(@a, 1, :exists), True, ':exists';
is postcircumfix:<{ }>(%h, "b", :k), 'b', ':k';
is postcircumfix:<[ ]>(@a, 2, :v), 3, ':v';
is-deeply postcircumfix:<{ }>(%h, "a", :p), (a => 1), ':p';

# --- the syntax form agrees with the routine form ---
throws-like { @a[0, 1]:foo }, X::Adverb, what => 'slice', 'the syntax slice form';
