use Test;

# Numeric coercion of Nil (an undefined value) warns "Use of Nil in numeric
# context" and resumes with the corresponding numeric ZERO — raku's `Nil.Int`
# is `0` (a defined Int), `Nil.Num` is `0e0`, `Nil.Rat` is `0.0`, etc. — NOT
# the type object.
#
# Regression: mutsu resumed with the numeric type object (`(Int)`), so
# `say Nil.Int` printed `(Int)` and `Nil.Int.defined` was False.

plan 14;

# `quietly { ... }` swallows the "Use of Nil in numeric context" warning; we
# assert the resumed value.

# --- values ---
is (quietly { Nil.Int }),     0,      'Nil.Int is 0';
is (quietly { Nil.Numeric }), 0,      'Nil.Numeric is 0';
is (quietly { Nil.Num }),     0e0,    'Nil.Num is 0e0';
is (quietly { Nil.Rat }),     0.0,    'Nil.Rat is 0.0';
is (quietly { Nil.Complex }), <0+0i>, 'Nil.Complex is 0+0i';

# --- types ---
is (quietly { Nil.Int }).WHAT.^name,     'Int',     'Nil.Int is an Int';
is (quietly { Nil.Num }).WHAT.^name,     'Num',     'Nil.Num is a Num';
is (quietly { Nil.Rat }).WHAT.^name,     'Rat',     'Nil.Rat is a Rat';
is (quietly { Nil.FatRat }).WHAT.^name,  'FatRat',  'Nil.FatRat is a FatRat';
is (quietly { Nil.Complex }).WHAT.^name, 'Complex', 'Nil.Complex is a Complex';

# --- the resumed zero is a defined, usable value ---
ok (quietly { Nil.Int }).defined, 'Nil.Int is defined (a real 0, not a type object)';
is (quietly { Nil.Int + 5 }), 5, 'Nil.Int participates in arithmetic as 0';
is (quietly { Nil.FatRat }), 0, 'Nil.FatRat numifies to 0';

# --- a failed match numifies through Nil the same way ---
is (quietly { ("one-two" ~~ /234/).Int }), 0, 'failed-match Nil coerces to 0 via .Int';
