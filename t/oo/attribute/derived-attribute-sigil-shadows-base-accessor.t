use v6;
use Test;

plan 9;

# A derived class's attribute of the same name but a different sigil provides
# the accessor (and the value) for the derived class; the base keeps its own.
class P { has $.v = "p" }
class Q is P { has @.v = <q1 q2> }
class S is P { has %.v = a => 1 }
class T is Q { has $.v = "t" }
class R is P { has $.v = "r" }

is-deeply Q.new.v, ["q1", "q2"], '@.v in the derived class shadows $.v';
is P.new.v, "p", 'the base class keeps its own';
is R.new.v, "r", 'same sigil still shadows';
is-deeply S.new.v, {:a(1)}, '%.v shadows $.v';
is T.new.v, "t", '$.v shadows an inherited @.v';
is Q.new(v => <a b>).v.join(","), "a,b", 'a named argument fills the derived attribute';
is-deeply Q.new(v => <a b>).v.elems, 2, 'and has the derived shape';
is P.new(v => "x").v, "x", 'a named argument for the base';
is-deeply T.new(v => "u").v, "u", 'a named argument for the most-derived scalar';
