use v6;
use Test;

# `unique`, `squish` and `repeated` are list-prefix builtins (like `reverse`
# and `sort`): they take arguments without parentheses, including a `<...>`
# word-quote term. Previously `unique <1 2 3>` failed with
# "Confused. Two terms in a row" because the parser did not recognise these
# names as list operators, so the `<...>` was not gobbled as an argument.

plan 12;

# The regressed case: a word-quote term as the sole listop argument.
is (unique <1 2 2 3 3 3>).gist,   '(1 2 3)', 'unique <...>';
is (squish <1 1 2 2 1>).gist,     '(1 2 1)', 'squish <...>';
is (repeated <1 2 2 3 3>).gist,   '(2 3)',   'repeated <...>';

# The comma-list and parenthesized forms keep working.
is (unique 1, 2, 2, 3).gist,      '(1 2 3)', 'unique comma list';
is unique(1, 2, 2, 3).gist,       '(1 2 3)', 'unique(...)';
is (squish 1, 1, 2, 2).gist,      '(1 2)',   'squish comma list';
is squish(1, 1, 2).gist,          '(1 2)',   'squish(...)';
is (repeated 1, 2, 2, 3, 3).gist, '(2 3)',   'repeated comma list';

# Adverbs still parse after the listop head.
is (unique :as(*.lc), <a A b B c>).gist, '(a b c)', 'unique :as adverb + word-quote';
is (unique <1 2 2 3>, :with(&[==])).gist, '(1 2 3)', 'unique word-quote + :with adverb';

# The method form is unaffected.
is <1 2 2 3 3>.unique.gist,   '(1 2 3)', '.unique method';
is <1 1 2 2 1>.squish.gist,   '(1 2 1)', '.squish method';
