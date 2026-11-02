use Test;

# Found via DAWG: `$str.trans(%unicode-map)` takes a Hash as its list of
# changes (the Hash flattens into its pairs), and mutsu used to refuse it as a
# non-Pair argument.
plan 8;

my %m = a => "1", b => "2";
is "abca".trans(%m), "12c1", 'a Hash argument is its pairs';
is "abca".trans(%m.pairs), "12c1", 'the same pairs spelled out';
is "xaby".trans(%m, :delete), "x12y", 'with an adverb';
is "abc".trans(('a'..'c') => ('x'..'z')), "xyz", 'a Pair of ranges still works';
is "a-b".trans(%(), :delete), "a-b", 'an empty Hash changes nothing';
my %r = a => "x", b => "y", c => "z";
is "abcab".trans(%r), "xyzxy", 'every pair of a multi-entry Hash applies';
my %adverbish = s => "S", d => "D", c => "C";
is "scd".trans(%adverbish), "SCD", 'keys spelled s, d, c translate (they are not adverbs)';
throws-like { "a".trans(rx/a/) }, X::Str::Trans::InvalidArg, 'a bare Regex is still refused';
