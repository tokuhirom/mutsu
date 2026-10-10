use Test;

# Supply.list is a row of the method table (ADR-11276 §9.55).

plan 3;

is Supply.from-list(1, 2, 3).list.raku, '(1, 2, 3)', 'a materialized supply lists its values';
is (supply { emit 5; emit 6 }).list.raku, '(5, 6)', 'an on-demand supply runs its body';
is Supply.from-list(1, 2).Array.raku, '[1, 2]', '.Array still builds an Array';
