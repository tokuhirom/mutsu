use Test;

plan 26;

# Range.min/.max with :k/:v/:kv/:p adverbs report the index (key) of the
# extremum element, not just the value. The extremum of a Range is its first
# (min) / last (max) element.
my $r := 2..6;   # elements 2,3,4,5,6 at indices 0..4

is-deeply $r.min(:k),   0,             '2..6 min :k';
is-deeply $r.min(:!k),  2,             '2..6 min :!k (value)';
is-deeply $r.min(:v),   2,             '2..6 min :v';
is-deeply $r.min(:kv),  (0, 2),        '2..6 min :kv';
is-deeply $r.min(:p),   Pair.new(0,2), '2..6 min :p';

is-deeply $r.max(:k),   4,             '2..6 max :k';
is-deeply $r.max(:!k),  6,             '2..6 max :!k (value)';
is-deeply $r.max(:kv),  (4, 6),        '2..6 max :kv';
is-deeply $r.max(:p),   Pair.new(4,6), '2..6 max :p';

# Sub forms.
is-deeply min($r, :k),  0,             'min 2..6 :k';
is-deeply min($r, :kv), (0, 2),        'min 2..6 :kv';
is-deeply max($r, :k),  4,             'max 2..6 :k';
is-deeply max($r, :kv), (4, 6),        'max 2..6 :kv';
is-deeply max($r, :p),  Pair.new(4,6), 'max 2..6 :p';

# Infinite range: max sits at index Inf with value Inf.
my $i := 2..Inf;
is-deeply $i.min(:k),   0,                 '2..Inf min :k';
is-deeply $i.min(:kv),  (0, 2),            '2..Inf min :kv';
is-deeply $i.max(:k),   Inf,               '2..Inf max :k';
is-deeply $i.max(:kv),  (Inf, Inf),        '2..Inf max :kv';
is-deeply $i.max(:p),   Pair.new(Inf,Inf), '2..Inf max :p';

# Lists report ALL matching indices as a list (distinct from Range's scalar).
is-deeply (3, 1, 2, 1).min(:k),  (1, 3),   'list min :k (all indices)';
is-deeply (3, 1, 2).max(:k),     (0,),     'list max :k';
is-deeply (3, 1, 2).min(:kv),    (1, 1),   'list min :kv';
is-deeply (3, 1, 2).max(:p),     (0 => 3,), 'list max :p';

# Plain min/max unaffected.
is (5, 2, 8, 1).min, 1, 'plain list min';
is $r.max, 6, 'plain range max';
is $i.max, Inf, 'plain infinite range max';

done-testing;
