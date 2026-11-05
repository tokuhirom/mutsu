use v6;
use Test;

# `.Array` of a List or Seq makes a fresh Array whose slots default to Any, so
# a Nil element decays to Any. Seen in the ClickHouse dist, whose
# `coerce-value([Nil, 3], 'Array(Nullable(Int32))')` maps a Nullable to Nil and
# calls `.Array` on the Seq.

plan 6;

is-deeply (Nil, 3).Array, [Any, 3], 'List.Array';
is-deeply (Nil, 3).list.Array, [Any, 3], 'list.Array';
is-deeply (Nil, 3).Seq.Array, [Any, 3], 'Seq.Array';
is-deeply [Nil, 3].map({ $_ }).Array, [Any, 3], 'a map over Nil elements';
is-deeply [1, 2].map({ Nil }).Array, [Any, Any], 'a map block that returns Nil';
is-deeply [1, 2].map({ Nil }).list.raku, (Nil, Nil).list.raku, '.list keeps the Nil';
