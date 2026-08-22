use v6;
use Test;

# .isa() checks only the nominal class hierarchy (`.^mro`); it must be False
# for a role a value merely *does* (composes), even when it's True for
# .does()/~~. Every expectation below was verified against real `raku`
# (2026-08-22). See todo/deep/isa-conflates-roles-with-nominal-supertypes.md
# (now retired to news/) for the audit.

plan 46;

# Numeric / Real / Rational are roles, not nominal ancestors of Int/Rat/etc.
is 42.isa(Numeric), False, 'Int does NOT isa Numeric (role, not nominal)';
is 42.does(Numeric), True, 'Int does Numeric (role-aware)';
is (42 ~~ Numeric), True, 'Int ~~ Numeric (role-aware)';
is 42.isa(Real), False, 'Int does NOT isa Real (role, not nominal)';
is 42.does(Real), True, 'Int does Real (role-aware)';
is (1/2).isa(Rational), False, 'Rat does NOT isa Rational (role, not nominal)';
is (1/2).does(Rational), True, 'Rat does Rational (role-aware)';
is 42.isa(Cool), True, 'Int isa Cool (Cool IS a real ancestor)';

# Stringy is a role, not a nominal ancestor of Str.
is 'x'.isa(Stringy), False, 'Str does NOT isa Stringy (role, not nominal)';
is 'x'.does(Stringy), True, 'Str does Stringy (role-aware)';

# Dateish is a role, not a nominal ancestor of Date/DateTime.
is Date.today.isa(Dateish), False, 'Date does NOT isa Dateish (role, not nominal)';
is Date.today.does(Dateish), True, 'Date does Dateish (role-aware)';

# Positional / Associative / Iterable are roles.
is [1, 2].isa(Positional), False, 'Array does NOT isa Positional (role, not nominal)';
is [1, 2].does(Positional), True, 'Array does Positional (role-aware)';
is {a => 1}.isa(Associative), False, 'Hash does NOT isa Associative (role, not nominal)';
is {a => 1}.does(Associative), True, 'Hash does Associative (role-aware)';
is (1 .. 3).isa(Iterable), False, 'Range does NOT isa Iterable (role, not nominal)';

# Map IS a real nominal ancestor of Hash, but NOT of Pair/Set/Bag/Mix/Capture
# (those only compose the Associative role).
is {a => 1}.isa(Map), True, 'Hash isa Map (Map IS a real ancestor)';
is (1 => 2).isa(Map), False, 'Pair does NOT isa Map (Map is not an ancestor of Pair)';
is (1 => 2).does(Map), False, 'Pair does NOT does(Map) either (Map is nominal-only)';
is (1 => 2).does(Associative), True, 'Pair does Associative (role-aware)';
is Set.new.isa(Map), False, 'Set does NOT isa Map';
is Set.new.does(Associative), True, 'Set does Associative (role-aware)';
is Capture.new.isa(Map), False, 'Capture does NOT isa Map';

# Callable is a role; Block/Routine/Code are real nominal ancestors of Sub.
my $sub = sub () { 42 };
is $sub.isa(Callable), False, 'Sub does NOT isa Callable (role, not nominal)';
is $sub.does(Callable), True, 'Sub does Callable (role-aware)';
is $sub.isa(Code), True, 'Sub isa Code (Code IS a real ancestor)';
is $sub.isa(Block), True, 'Sub isa Block (Block IS a real ancestor)';
is $sub.isa(Routine), True, 'Sub isa Routine (Routine IS a real ancestor)';

class ISADoesSplitFoo { method bar() { 42 } }
my $method = ISADoesSplitFoo.new.^find_method('bar');
is $method.isa(Method), True, 'Method isa Method';
is $method.isa(Routine), True, 'Method isa Routine (Routine IS a real ancestor)';
is $method.isa(Callable), False, 'Method does NOT isa Callable (role, not nominal)';

# Cool IS a real ancestor of Match, but NOT of a bare Capture.
my $match = 'abc' ~~ /a/;
is $match.isa(Cool), True, 'Match isa Cool (Cool IS a real ancestor of Match)';
is Capture.new.isa(Cool), False, 'Capture does NOT isa Cool (Cool is not an ancestor of Capture)';

# A genuine forced Seq isa Seq but NOT List; a lazy-array (my @a = lazy ...)
# isa List (and Array) but NOT Seq — mutsu must not conflate the two shapes.
my $mapped = (1, 2, 3).map({ $_ * 2 });
is $mapped.isa(Seq), True, 'map() result isa Seq';
is $mapped.isa(List), False, 'map() result does NOT isa List';
my @lazy-arr = lazy 1, 2, 3;
is @lazy-arr.isa(List), True, 'lazy array isa List';
is @lazy-arr.isa(Array), True, 'lazy array isa Array';
is @lazy-arr.isa(Seq), False, 'lazy array does NOT isa Seq';

# HyperSeq/RaceSeq do not nominally descend from Seq (verified against real
# raku's genuine HyperSeq). mutsu's own `.hyper()` builtin is a separate,
# pre-existing gap: it is actually backed by ValueView::Array internally (only
# `.^name`/`.WHAT` fake the "HyperSeq" display name through a different
# mechanism), so `.isa(List)` on it legitimately follows Array's real ancestry
# and is intentionally NOT asserted False here.
my $hyper = (1, 2, 3).hyper;
is $hyper.isa(Seq), False, 'HyperSeq-shaped value does NOT isa Seq';

# Bool IS a real nominal ancestor-descendant of Int (Bool.^mro includes Int).
is True.isa(Int), True, 'Bool isa Int (Bool.^mro includes Int)';

# Array < List nominally.
is [1, 2, 3].isa(List), True, 'Array isa List';
is [1, 2, 3].isa(Seq), False, 'Array does NOT isa Seq';

# SetHash/BagHash/MixHash stay concrete/nominal identity checks (unaffected
# by the role split — they were already correct).
is SetHash.new.isa(SetHash), True, 'SetHash isa SetHash';
is SetHash.new.isa(Set), False, 'SetHash does NOT isa Set (sibling classes, not ancestor/descendant)';

# Exception family stays nominal (unaffected by the split).
is X::AdHoc.new(payload => 'x').isa(Exception), True, 'X::AdHoc isa Exception';

done-testing;
