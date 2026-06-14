use v6;
use Test;

# VM-native default construction now covers classes with `where`-constrained
# attributes (Track A ③ constructor). The instance is assembled natively, then
# `where` predicates are enforced as a post-assembly phase — at attribute
# *assignment* time (before TWEAK), matching the full constructor and raku.

plan 14;

# --- a satisfied / violated where ---
class Pos { has Int $.n where * > 0; }
is Pos.new(n => 5).n, 5, 'a value satisfying its where is accepted';
ok (try Pos.new(n => 3)).defined, 'positive value constructs';
nok (try Pos.new(n => -1)).defined, 'a value failing its where is rejected';

# --- where on a defaulted attribute ---
class D { has Int $.x where * >= 0 = 10; }
is D.new.x, 10, 'a default value that satisfies where is accepted';
ok (try D.new).defined, 'defaulted where-attribute constructs';

# --- untyped where ---
class U { has $.s where *.chars > 2; }
is U.new(s => "abc").s, "abc", 'untyped where on a string attribute';
nok (try U.new(s => "a")).defined, 'short string fails the where';

# --- multiple attributes, one constrained ---
class M { has $.a; has Int $.b where * %% 2; }
is M.new(a => 1, b => 4).b, 4, 'an even value satisfies %%2';
nok (try M.new(a => 1, b => 3)).defined, 'an odd value fails %%2';

# --- where is checked BEFORE TWEAK runs (assignment-time semantics) ---
class T { has Int $.v where * > 100; submethod TWEAK { $!v = $!v * 1000 } }
nok (try T.new(v => 1)).defined,
    'where rejects at assignment time; a fixing TWEAK never runs';

# --- a valid initial value lets TWEAK run, post-TWEAK still satisfies where ---
class T2 { has Int $.v where * > 100; submethod TWEAK { $!v = $!v + 1 } }
is T2.new(v => 200).v, 201, 'TWEAK runs when the initial value passes where';
ok (try T2.new(v => 200)).defined, 'where + TWEAK constructs with a valid value';

# --- a TWEAK that mutates INTO a violation is rejected post-TWEAK ---
class T3 { has Int $.v where * > 100; submethod TWEAK { $!v = 1 } }
nok (try T3.new(v => 200)).defined, 'TWEAK mutating into a where violation is rejected';

# --- subset-typed attribute (where via a subset) ---
subset Even of Int where * %% 2;
class S { has Even $.e; }
is S.new(e => 8).e, 8, 'subset-typed attribute accepts a matching value';
