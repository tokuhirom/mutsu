use Test;

plan 10;

# A destructuring signature binds its inner names exactly as a flat one does, so the
# body must see them as declared terms -- not as the head of a listop call. `u %% v`
# used to parse as `u(%% v)` and die with "Unknown function: u".
my &divisible = -> [ \a, \u, \v ] { u %% v };
ok divisible([1, 4, 2]), 'a destructured sigilless name is a term, not a listop';
nok divisible([1, 3, 2]), 'the destructured names carry the right values';

# Flat sigilless params already worked; they must keep working.
my &flat = -> \u, \v { u %% v };
ok flat(4, 2), 'flat sigilless params are unchanged';

# Sigiled destructuring, for comparison.
my &sigiled = -> [$a, $u, $v] { $u %% $v };
ok sigiled([1, 4, 2]), 'a sigiled destructuring signature binds too';

# `grep` used to bind its block's parameters by name straight into the env, which cannot
# take an element apart -- so a destructuring signature left the inner names unbound.
my @triples = ([1, 4, 2], [1, 3, 2], [5, 9, 3]);
is-deeply @triples.grep(-> [ \a, \u, \v ] { u %% v }).List,
    ([1, 4, 2], [5, 9, 3]),
    'grep binds a destructuring signature';

is-deeply @triples.grep(-> [$a, $u, $v] { $u %% $v }).List,
    ([1, 4, 2], [5, 9, 3]),
    'grep binds a sigiled destructuring signature';

# `map` already went through the real binder; it must keep agreeing with grep.
is-deeply @triples.map(-> [ \a, \u, \v ] { u div v }).List,
    (2, 1, 3),
    'map binds a destructuring signature';

# A plain grep block is unaffected.
is-deeply (1..6).grep(-> $n { $n %% 2 }).List, (2, 4, 6), 'a plain grep block is unchanged';

# `grep` promotes each matched source slot to a shared ContainerRef cell so a writeback
# loop can mutate through it. A later destructuring bind has to look through that cell.
my @cells = ([1, 4, 2], [1, 3, 2]);
@cells.grep({ .elems == 3 });
is-deeply @cells.map(-> [ \a, \u, \v ] { u }).List, (4, 3),
    'destructuring still binds after grep containerized the source slots';

@cells.grep(-> [ \a, \u, \v ] { u %% v });
is-deeply @cells.grep(-> [ \a, \u, \v ] { u %% v }).List, ([1, 4, 2],),
    'a second destructuring grep over the same array still binds';
