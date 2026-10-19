use Test;

# A user method on a subclass of a built-in type defers to the built-in method
# of the same name: in Rakudo that method is a real candidate of the MRO, so
# `callsame`/`nextsame` out of the override reaches it (ADR-11276 slice 4).
# These answered Nil before the built-in became the last deferral candidate.

plan 16;

class S is Str { method uc { '[' ~ callsame() ~ ']' } }
is S.new(value => 'ab').uc, '[AB]', 'callsame into Str.uc';

class I is Int { method succ { nextsame } }
is I.new(4).succ, 5, 'nextsame into Int.succ';

class L is List { method elems { 10 + callsame } }
is L.new(1, 2).elems, 12, 'callsame into List.elems';

class P is Pair { method key { 'k:' ~ callsame } }
is P.new(key => 'x', value => 1).key, 'k:x', 'callsame into Pair.key';

class C is Cool { method uc { 'c' ~ callsame } }
like C.new.uc, /^ 'cC<' \d+ '>' $/, 'callsame into Cool.uc reads the instance as a Str';

class F is Num { method floor { nextsame() + 1 } }
is F.new(2.5e0).floor, 2, 'nextsame into Num.floor leaves the method';

class R is Rat { method Str { 'rat:' ~ callsame } }
is R.new(1, 2).Str, 'rat:0.5', 'callsame into Rat.Str';

class D is Date { method Str { 'd:' ~ callsame } }
is D.new('2020-01-02').Str, 'd:2020-01-02', 'callsame into Date.Str';

class M is Map { method keys { callsame.sort } }
is M.new((b => 1, a => 2)).keys, <a b>, 'callsame into Map.keys';

class SB is SetHash { method elems { 100 + callsame } }
is SB.new(<a b>).elems, 102, 'callsame into SetHash.elems';

class B is Bag { method total { 1000 + nextsame } }
is B.new(<a a b>).total, 3, 'nextsame leaves the method: Bag.total';

class A is Array { method push(|c) { callsame } }
my $a = A.new;
$a.push(1);
is $a.elems, 1, 'callsame into Array.push still writes the storage';

class U { method elems { callsame } }
is U.new.elems, 1, 'callsame from a plain class reaches Any.elems';

class K is Str { method lc { my $r = callsame; $r ~ $r } }
is K.new(value => 'Ab').lc, 'abab', 'callsame returns the value, not a container';

class W is Str { method chars { nextcallee()(self) * 10 } }
is W.new(value => 'abc').chars, 30, 'nextcallee hands back the built-in method';
class V is Int { method abs { my &next = nextcallee; next(self) + 1 } }
is V.new(-3).abs, 4, 'nextcallee into Int.abs';
