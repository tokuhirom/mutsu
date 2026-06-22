use Test;

# A real (`@`-sigil) array whose `.raku` has a single element that is itself an
# Iterable (Range, List/Array, Seq, Hash/Map) renders with a trailing comma so
# the round-trip does not flatten it: `[1..5,]`, `[(1, 2),]`, `[{:x(1)},]`.
# Pair, Set/Bag (constructor-call forms), scalars, and type objects do NOT.

plan 18;

# --- single Iterable element => trailing comma -----------------------------

{ my @a = 1..5,;            is @a.raku, '[1..5,]',            'single Range'; }
{ my @a = $(1, 2),;         is @a.raku, '[(1, 2),]',          'single itemized List'; }
{ my @a = $[1, 2],;         is @a.raku, '[[1, 2],]',          'single itemized Array'; }
{ my @a = %(x => 1),;       is @a.raku, '[{:x(1)},]',         'single Hash'; }
{ my @a = (1..5).Seq,;      is @a.raku, '[(1, 2, 3, 4, 5).Seq,]', 'single Seq'; }
is [[1, 2, 3],].raku,       '[[1, 2, 3],]',                   'array literal: single Array';
{ my @a = [1, 2, 3],;       is @a.raku, '[[1, 2, 3],]',       'single Array element'; }

# --- single non-Iterable element => NO trailing comma ----------------------

{ my @a = 42,;              is @a.raku, '[42]',               'single Int: no comma'; }
{ my @a = "s",;             is @a.raku, '["s"]',              'single Str: no comma'; }
{ my @a = (1 => 2),;        is @a.raku, '[1 => 2]',           'single Pair: no comma'; }
{ my @a = Any,;             is @a.raku, '[Any]',              'single type object: no comma'; }

# --- itemized array ($[...]) follows the same rule -------------------------

is $[$(1, 2)].raku,         '$[(1, 2),]',                     'item-array: single listy';
is $[42].raku,              '$[42]',                          'item-array: single scalar';

# --- regressions guard: unchanged cases ------------------------------------

is [].raku,                 '[]',                             'empty array';
is [1, 2, 3].raku,          '[1, 2, 3]',                      'multi scalar';
is [[1, 2], [3, 4]].raku,   '[[1, 2], [3, 4]]',               'multi array';
is (1, 2, 3).raku,          '(1, 2, 3)',                      'multi-element list';
is (1..5,).raku,            '(1..5,)',                        'single-element list keeps comma';
