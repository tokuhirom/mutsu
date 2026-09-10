use Test;

# .reduce on an *undefined* invocant (a bare type object or Nil) is Nil, because
# Any.reduce is guarded on a defined invocant. .reduce on an empty concrete list
# returns the operator's identity element (0 for +, "" for ~, 1 for *, …), like
# the [op] reduce metaop and matching raku.

plan 14;

# --- undefined invocant => Nil --------------------------------------------
is Range.reduce(&infix:<+>), Nil, 'Range.reduce (type object) is Nil';
is Str.reduce(&infix:<~>),   Nil, 'Str.reduce (type object) is Nil';
is Array.reduce(&infix:<+>), Nil, 'Array.reduce (type object) is Nil';
is Int.reduce(&infix:<+>),   Nil, 'Int.reduce (type object) is Nil';
is Any.reduce(&infix:<+>),   Nil, 'Any.reduce (type object) is Nil';

# --- empty concrete list => operator identity -----------------------------
is [].reduce(&infix:<+>), 0,  'empty list .reduce(&+) is 0';
is [].reduce(&infix:<~>), '', 'empty list .reduce(&~) is ""';
is [].reduce(&infix:<*>), 1,  'empty list .reduce(&*) is 1';
my @e;
is @e.reduce(&infix:<+>), 0, 'empty @array .reduce(&+) is 0';
is (reduce &infix:<+>, ()), 0, 'reduce &+, () (function form) is 0';

# --- a type object as a list *element* reduces normally (not the guard) ----
is (Range,).reduce(&infix:<+>), 0, 'a type object element coerces (0), not Nil';

# --- non-empty still reduces ----------------------------------------------
is [1, 2, 3].reduce(&infix:<+>), 6, 'non-empty .reduce still works';
is (1..5).reduce(&infix:<+>), 15, 'Range .reduce still works';
is [5].reduce(&infix:<+>), 5, 'single-element .reduce returns the element';
