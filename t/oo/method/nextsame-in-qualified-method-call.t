use Test;

plan 6;

# A qualified call (`self.R::m`, `self.Q::m`) defers along the QUALIFIER's own
# chain, never the receiver's (#11592). A role's method is in no class's
# chain, so `nextsame` in it answers Nil; it used to run the receiver's next
# MRO candidate instead.

my @log;
role R { method m() { @log.push: self.^name; nextsame } }
class Base { method m() { @log.push: 'base' } }
class C is Base does R { method m() { self.R::m() } }

C.m;
is-deeply @log, ['C'], 'nextsame in a role-qualified call does not reach the receiver\'s parent';
@log = ();
C.new.m;
is-deeply @log, ['C'], '... on an instance too';

role R2 { method new(*%a) { nextsame } }
class C2 does R2 { has $.thing; method new(*%a) { self.R2::new(|%a) } }
is C2.new(thing => 1), Nil, 'a role-qualified constructor has no base `new` to defer to';

# A class qualifier defers along that class's own MRO.
class P { method m { 'P' } }
class Q is P { method m { 'Q ' ~ callsame } }
class S is Q { method m { self.Q::m } }
is S.new.m, 'Q P', 'callsame in a class-qualified call reaches the qualifier\'s parent';

# An unqualified call on a punned role still reaches the native base `new`.
role RP { has $.x; method new(*%a) { nextsame } }
is RP.new(x => 3).x, 3, 'nextsame in a punned role\'s own new still constructs';

role R3 { method m { 'R3 ' ~ (callsame() // 'none') } }
is R3.m, 'R3 none', 'callsame in a punned role method finds nothing';
