use v6;
use Test;

# The builtin-type catalog's MRO rows are interned ONCE per process
# (`builtin_type_mro_syms` / `builtin_type_mro_ids`, #7766) instead of being
# re-interned into a fresh chain on every dispatch that reaches them. The memo
# is shared by every caller, so a mistake here is a *wrong ancestry* — a
# dropped, reordered or cross-wired ancestor — not a slowdown: it would change
# which class answers a method, not how fast it answers.
#
# Every expectation below is raku's own `.^mro` (Rakudo 2026.06).

plan 24;

# --- the catalog rows themselves ---------------------------------------------

sub mro-of(Mu $t) { $t.^mro.map(*.^name).join(" ") }

is mro-of(Str), "Str Cool Any Mu", 'Str keeps its own ancestry';
is mro-of(Int), "Int Cool Any Mu", 'Int keeps its own ancestry';
is mro-of(Bool), "Bool Int Cool Any Mu", 'Bool inherits through Int';
is mro-of(Hash), "Hash Map Cool Any Mu", 'Hash inherits through Map';
is mro-of(Array), "Array List Cool Any Mu", 'Array inherits through List';
is mro-of(Map), "Map Cool Any Mu", 'Map is a sibling of Hash, not its child';
is mro-of(Range), "Range Cool Any Mu", 'Range keeps its own ancestry';
is mro-of(Seq), "Seq Cool Any Mu", 'Seq keeps its own ancestry';
is mro-of(Pair), "Pair Any Mu", 'Pair does not inherit Cool';
is mro-of(Junction), "Junction Mu", 'Junction skips Any';

# A shared memo that handed out a chain the first caller could mutate would
# show up as a second reader seeing a different answer.
is mro-of(Str), "Str Cool Any Mu", 'a repeat read of a row is unchanged';
is mro-of(Bool), "Bool Int Cool Any Mu", 'a repeat read of a longer row is unchanged';

# --- chains spliced on top of a catalog row ----------------------------------

class ArrayHeir is Array { }
is mro-of(ArrayHeir), "ArrayHeir Array List Cool Any Mu",
    'a user class deriving from a builtin gets the catalog tail';

class StrHeir is Str { }
is mro-of(StrHeir), "StrHeir Str Cool Any Mu",
    'a second heir of a different builtin gets its own tail, not the first one';

# --- the MOP uses the same ancestry for values and type objects --------------
is-deeply "x".^isa(Cool), 1,
    'concrete Str values use the catalog for .^isa';
is-deeply True.^isa(Int), 1,
    'concrete Bool values use the catalog for .^isa';
is-deeply 1.5.Rat.^isa(Cool), 1,
    'concrete Rat values use the catalog for .^isa';
is mro-of(Array[Int]), "Array[Int] Array List Cool Any Mu",
    'a parametrized builtin type splices its base catalog MRO';

# --- the ancestry is the one dispatch actually uses ---------------------------
# `.^mro` could be right while the chain the method walk consults is wrong, so
# pin a call that can only resolve through a catalog ancestor.
#
# `.isa`, not `.^isa`: the MOP spelling answers False for a concrete builtin
# value (mutsu bug, #7937 -- `"x".^isa(Cool)` is 0 where raku says 1). Swap it
# back when that is fixed. `Array[Int].^mro` is missing from the rows above for
# the same reason: #7937's second half, where `.^mro` alone drops a
# parametrized name's base row that every other ancestry consumer splices in.

ok True.isa(Int), 'Bool is-a Int through the catalog chain';
ok 1.5.Rat.isa(Cool), 'Rat is-a Cool through the catalog chain';
nok (a => 1).isa(Cool), 'Pair is NOT a Cool';

use MONKEY-TYPING;
augment class Cool { method catalog-probe() { "cool/" ~ self.^name } }
is "x".catalog-probe, "cool/Str", 'a Str finds a method augmented onto Cool';
is 42.catalog-probe, "cool/Int", 'an Int finds the same method through its own chain';
is True.catalog-probe, "cool/Bool", 'a Bool reaches Cool through Int';
