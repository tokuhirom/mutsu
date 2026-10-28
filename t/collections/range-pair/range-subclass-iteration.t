use v6;
use Test;

plan 8;

# An instance of a `class ... is Range` subclass iterates the range it carries.
class Q is Range { }

my @seen;
@seen.push($_) for Q.new(1, 3);
is-deeply @seen, [1, 2, 3], '`for` iterates the carried range';

my @a = Q.new(1, 3);
is-deeply @a, [1, 2, 3], 'list assignment flattens it';

is-deeply Q.new(1, 3).map(* + 1).List, (2, 3, 4), '.map reads its elements';

my @b = |Q.new(1, 3);
is-deeply @b, [1, 2, 3], '`|` slips its elements';

is-deeply Q.new(1, 3).list, (1, 2, 3), '.list';
is Q.new(1, 3).elems, 3, '.elems';

@seen = ();
@seen.push($_) for Q.new(1, 3).reverse;
is-deeply @seen, [3, 2, 1], '.reverse';

my @c = [Q.new(1, 2)];
is-deeply @c, [1, 2], 'a one-element array literal flattens it';
