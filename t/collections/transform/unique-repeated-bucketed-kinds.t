use Test;

# `unique` / `repeated` bucket Rat, FatRat, Complex, Pair and plain object
# elements by identity (#9161); the answers must stay those of the `===`
# scan, including across the pairs that look alike but are not identical.

plan 20;

# --- Rat / FatRat ---------------------------------------------------------
is-deeply (<1/2>, <1/2>, <3/4>, <2/4>).unique.List, (<1/2>, <3/4>).List, 'Rat unique, 2/4 is 1/2';
is-deeply (<1/2>, <1/2>, <3/4>).repeated.List, (<1/2>,).List, 'Rat repeated';
is-deeply (0.5, 0.5e0, 1/2).unique.List, (0.5, 0.5e0).List, 'a Rat and a Num are not ===';
is (1/2, 1, 2/2).unique.elems, 3, 'a whole Rat is not an Int';
is-deeply (FatRat.new(1, 2), FatRat.new(1, 2), 1/2).unique.elems, 2, 'FatRat is not Rat';

# --- Complex --------------------------------------------------------------
is-deeply (1+2i, 1+2i, 2i, NaN+0i, NaN+0i).unique.elems, 3, 'Complex unique, NaN parts are one class';
is-deeply (1+2i, 1+2i).repeated.elems, 1, 'Complex repeated';

# --- Pair -----------------------------------------------------------------
is-deeply (1 => 2, 1 => 2, 2 => 3).unique.List, (1 => 2, 2 => 3).List, 'Pair unique by key and value';
is-deeply (a => 1, a => 2, a => 1).unique.List, (a => 1, a => 2).List, 'same key, different value';
is-deeply (:a(1), 'a' => 1, :a(1)).unique.elems, 1, 'named and positional Str-keyed Pair are one class';
is-deeply ((1 => 'x'), (1 => 'x'), (2 => 'x')).repeated.elems, 1, 'Pair repeated';

# --- plain objects: by identity, not by attributes --------------------------
class Point { has $.x; has $.y }
my $p = Point.new(x => 1, y => 2);
my $q = Point.new(x => 1, y => 2);
is ($p, $p, $q).unique.elems, 2, 'objects are distinct unless the same object';
is ($p, $q, $p, $q).repeated.elems, 2, 'objects repeated by identity';

# --- an object whose class overrides WHICH is decided by it -----------------
class Tagged {
    has $.tag;
    method WHICH() { ValueObjAt.new("Tagged|$!tag") }
}
my @t = Tagged.new(tag => 'a'), Tagged.new(tag => 'b'), Tagged.new(tag => 'a');
is @t.unique.elems, 2, 'user WHICH: equal WHICH values are one';
is @t.repeated.elems, 1, 'user WHICH: the repeat is found';

# --- value classes keep their own rules -------------------------------------
is (Date.new('2026-10-23'), Date.new('2026-10-23'), Date.new('2026-10-24')).unique.elems, 2,
    'Date is a value type';
is (v1.2, v1.2, v1.3).unique.elems, 2, 'Version is a value type';

# --- mixed kinds in one list --------------------------------------------------
my @mixed = 1, <1/2>, 'a', 1 => 2, 1, <1/2>, 'a', 1 => 2, 1+2i;
is-deeply @mixed.unique.List, (1, <1/2>, 'a', 1 => 2, 1+2i).List, 'mixed kinds, unique';
is-deeply @mixed.repeated.List, (1, <1/2>, 'a', 1 => 2).List, 'mixed kinds, repeated';

# --- a large input of a bucketed kind finishes -----------------------------
is (^20000).map({ $_ / 7 }).unique.elems, 20000, 'twenty thousand distinct Rats';
