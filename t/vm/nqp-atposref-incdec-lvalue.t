use Test;
use nqp;

# Found via Fasta: `++nqp::atposref_i(@counts, $i)` steps the element the op
# hands a reference to (prefix and postfix, native int array and plain array).
plan 6;

my int @c = 0 xx 3;
++nqp::atposref_i(@c, 1);
++nqp::atposref_i(@c, 1);
is @c.raku, 'array[int].new(0, 2, 0)', 'prefix ++ on a native int element';

nqp::atposref_i(@c, 2)++;
is @c.raku, 'array[int].new(0, 2, 1)', 'postfix ++ on a native int element';

--nqp::atposref_i(@c, 1);
is @c.raku, 'array[int].new(0, 1, 1)', 'prefix -- on a native int element';

is (++nqp::atposref_i(@c, 0)), 1, 'the prefix form answers the new value';
is (nqp::atposref_i(@c, 0)++), 1, 'the postfix form answers the old value';

my int @grow;
++nqp::atposref_i(@grow, 3);
is @grow.raku, 'array[int].new(0, 0, 0, 1)', 'an element past the end grows the array';
