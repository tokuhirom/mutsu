use Test;

plan 26;

# `first(&test, +values)` slurps under the single-argument rule, like `map` and
# `grep`: exactly one list argument is flattened into its elements unless it is
# itemized, and two or more list arguments are each ONE element of their own.
# (The listop form used to flatten every Array/List argument unconditionally.)

my @a = [1,2],;
my @b = 1, 2;
my $s = [1,2];
my $l = (1,2);

# --- an itemized argument is one item ----------------------------------------
is (first { True }, $([1,2])).^name,     'Array', 'first &t, $([1,2])';
is (first { True }, [1,2].item).^name,   'Array', 'first &t, [1,2].item';
is (first { True }, @a[0]).^name,        'Array', 'first &t, @a[0]: an element read is itemized';
is (first { True }, $s).^name,           'Array', 'first &t, $s holding an Array';
is (first { True }, $l).^name,           'List',  'first &t, $l holding a List';
is (first { True }, $(1,2)).^name,       'List',  'first &t, $(1,2)';
is (first { True }, (1,2).item).^name,   'List',  'first &t, (1,2).item';
is (first { .elems > 1 }, $(1,2,3)).raku, '$(1, 2, 3)', 'the itemized list reaches the test whole';
is (first { .elems > 1 }, $(1,2,3), 5).raku, '$(1, 2, 3)', 'itemized list followed by a scalar';
is (first { True }, 1, $(1,2)).^name,    'Int',   'a scalar before an itemized list comes first';
is (first { True }, $(1,2), 1).^name,    'List',  'an itemized list before a scalar comes first';
is (first { True }, $[1,2], :k).raku,    '0',     'an itemized argument with the :k adverb';

# --- a single bare list argument still flattens ------------------------------
is (first { True }, @b).^name,           'Int',   'first &t, @b flattens';
is (first { True }, [1,2]).^name,        'Int',   'first &t, [1,2] flattens';
is (first { True }, (1,2)).^name,        'Int',   'first &t, (1,2) flattens';
is (first { True }, 1..3).^name,         'Int',   'first &t, 1..3 flattens';
is (first { $_ > 1 }, @b),               2,       'first { $_ > 1 }, @b';
is (first { $_ > 1 }, (1,2,3), :k).raku, '1',     'a flattened list with the :k adverb';
is (first { True }, (1,2).Seq).^name,    'Int',   'first &t, a Seq flattens';
is (first { True }, slip(1,2)).^name,    'Int',   'first &t, a Slip flattens';
is (first { $_ > 1 }, 1, 2, 3),          2,       'first &t, 1, 2, 3: plain scalars';

# --- two or more list arguments are each one element -------------------------
is (first { True }, (1,2), (3,4)).^name, 'List',  'two Lists: each is one element';
is (first { True }, [1,2], [3,4]).^name, 'Array', 'two Arrays: each is one element';
is (first { True }, @b, @b).^name,       'Array', 'two @-variables: each is one element';
is (first { $_ ~~ Array }, 1, [2], 3).raku, '[2]', 'an Array among scalars is not flattened';
is (first { .sum > 10 }, (1,2), (3,9)).raku, '(3, 9)', 'the matching element is the whole list';
