use Test;

plan 9;

# The `bag` / `set` / `mix` list builtins flatten a Seq argument (e.g. from
# `.map` / `.comb`) into its individual elements, rather than counting the whole
# Seq as one opaque key.

is (set "abc".comb).keys.sort.join,        'abc',    'set flattens a .comb Seq';
is (bag "aabbc".comb).{'a'},               2,        'bag counts elements of a .comb Seq';
is (set (1, 2, 3).map(* + 1)).keys.sort.join(','), '2,3,4', 'set flattens a .map Seq';
is (bag (1, 1, 2).map(* + 0)).{'1'},       2,        'bag counts elements of a .map Seq';
is (mix <a b a>.map(*.uc)).{'A'},          2,        'mix flattens a .map Seq';

# Set difference over Seq-built sets/bags yields individual keys (advent2012-day13).
{
    my $words1 = bag "aa bb cc dd".comb(/\w+/);
    my $words2 = set "cc dd".comb(/\w+/);
    is ($words1 (-) $words2).keys.sort.join(','), 'aa,bb',
        'set difference over Seq-built quanthashes';
}

# Explicit flat lists are unaffected (regression guard).
is (set <x y z>).keys.sort.join,           'xyz',    'set of a flat list still works';
is (bag 1, 1, 2).{'1'},                    2,        'bag of a comma list still works';

# A single non-list value stays a single key.
is (set 'hello').keys.join,                'hello',  'set of one string is one key';
