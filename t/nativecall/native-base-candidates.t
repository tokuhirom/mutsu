use v6;
use MONKEY-TYPING;
use Test;

# The native base candidate of a deferral chain (ADR-11276 slice 4): the user
# MRO is exhausted and the builtin is the last candidate.
plan 4;

class Rounded is Array { method AT-POS($i) { nextwith($i.round) } }
is Rounded.new(10, 20, 30).AT-POS(1.4), 20, 'is Array subclass defers to the native storage';

class G { method gist { "<" ~ callsame() ~ ">" } }
is G.new.gist, '<G.new>', 'gist override defers to the native base';

class Lc { has $.x; method new(|c) { nextsame } }
is Lc.new(:x(5)).x, 5, 'new override defers to the native constructor';

augment class Str { method shout() { self.uc } }
is "ab".shout, "AB", 'augmented method on a core type';
