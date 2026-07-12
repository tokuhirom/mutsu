use v6;
use Test;

plan 12;

# Hash/Set/Bag/Mix invocants classify their pairs (surfaced by zef:
# Zef::Repository::Ecosystems.search does
# `%specs.classify({ .value.from-matcher })`).

my %h = a => 1, b => 2;
my %by-value = %h.classify({ .value });
is %by-value{1}[0].key, 'a', 'Hash.classify iterates pairs (key)';
is %by-value{1}[0].value, 1, 'Hash.classify iterates pairs (value)';
is %by-value{2}[0].key, 'b', 'Hash.classify second bucket';

my %cat = %h.categorize({ .value });
is %cat{1}[0].key, 'a', 'Hash.categorize iterates pairs';

my %set-buckets = set(<a b>).classify({ .key });
is %set-buckets<a>[0].value, True, 'Set.classify gets elem => True pairs';

my %bag-buckets = bag(<a a b>).classify({ .value });
is %bag-buckets{2}[0].key, 'a', 'Bag.classify gets elem => count pairs';
is %bag-buckets{1}[0].key, 'b', 'Bag.classify count-1 bucket';

# A bare fat-arrow pair list element reaches the classifier as the topic
# (it must not be swallowed as a named argument to the block).
my %pairs = (a => 1, b => 2).classify({ .value });
is %pairs{1}[0].key, 'a', 'list-of-pairs classify passes Pair as topic';
is %pairs{2}[0].value, 2, 'list-of-pairs classify second bucket';

my %single = (a => 1).classify({ .value });
is %single{1}[0].key, 'a', 'single Pair invocant classify';

# :as mapper also receives the Pair as its topic
my %as-mapped = (a => 1, b => 2).classify({ .value }, :as({ .key.uc }));
is %as-mapped{1}[0], 'A', ':as mapper receives Pair topic';
is %as-mapped{2}[0], 'B', ':as mapper second bucket';

done-testing;
