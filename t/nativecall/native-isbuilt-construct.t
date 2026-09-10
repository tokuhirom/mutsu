use v6;
use Test;

# VM-native default construction now covers classes whose attributes carry an
# `is built(Bool)` trait (Track A ③ constructor). The trait only flips whether
# an attribute is populated from a named argument; the native builder already
# honours it via `is_attribute_buildable` in its named-arg loop (an
# `is built(False)` attribute is skipped there and then gets its default in the
# fill loop), so it stays native. A MOP `Attribute.set_build` *closure* still
# replaces plain data assignment and keeps falling through to the interpreter.

plan 14;

# --- is built(False): a named arg does NOT populate the attribute ---
class A { has $.x is built(False); }
is A.new(x => 5).x.^name, 'Any', 'is built(False) ignores a named arg';
is A.new.x.^name, 'Any', 'is built(False) with no arg is the type object';

# --- is built(False) with a default: the default still applies ---
class B { has $.y is built(False) = 99; }
is B.new(y => 5).y, 99, 'is built(False) keeps its default over a named arg';
is B.new.y, 99, 'is built(False) default with no arg';

# --- is built(True) / bare is built: normal named binding ---
class C { has $.z is built(True); }
is C.new(z => 7).z, 7, 'is built(True) binds a named arg';
class F { has $.p is built; }
is F.new(p => 8).p, 8, 'bare is built binds a named arg';

# --- is built(False) on a typed attribute -> declared type object ---
class D { has Int $.n is built(False); }
is D.new(n => 3).n.^name, 'Int', 'is built(False) typed attribute is its type object';

# --- a private is built(False) attribute, read inside a method ---
class G { has $!h is built(False); method peek() { $!h.^name } }
is G.new.peek, 'Any', 'private is built(False) attribute is the type object';

# --- is built(False) sibling of a normal attribute ---
class E { has $.a is built(False); has $.b; }
my $e = E.new(a => 1, b => 2);
is $e.a.^name, 'Any', 'is built(False) sibling is unset';
is $e.b, 2, 'normal sibling is set';

# --- is built(False) + BUILD that sets the attribute explicitly ---
class H { has $.x is built(False); submethod BUILD() { $!x = 11 } }
is H.new(x => 5).x, 11, 'BUILD sets an is built(False) attribute, named arg ignored';

# --- inheritance: an inherited is built(False) attribute keeps its default ---
class PBase { has $.base is built(False) = 7; }
class CDer is PBase { has $.own; }
my $c = CDer.new(base => 99, own => 3);
is $c.base, 7, 'inherited is built(False) keeps its default';
is $c.own, 3, 'own attribute binds normally';

# --- is built(True) combined with a typed where constraint ---
class W { has Int $.v is built(True) where * > 0; }
is W.new(v => 5).v, 5, 'is built(True) typed where-constrained attribute';
