use Test;

# Raku assigns to an `@`-sigil attribute exactly the way `my @a = …` assigns:
# list assignment. Anything Positional/iterable flattens into the container, a
# Hash flattens to its pairs, Nil becomes [Any], and a plain scalar or type
# object becomes a one-element array. mutsu used to wrap a non-array-*shaped*
# default expression at parse time (so `has @.w = 1..3` kept the Range as one
# element) and to pass a supplied scalar straight through uncoerced.

plan 26;

class W { has @.w = 1..3 }
is-deeply W.new.w, [1, 2, 3], 'a Range default list-assigns';

class Z { has @.z = (1, 2, 3).Seq }
is-deeply Z.new.z, [1, 2, 3], 'a Seq default list-assigns';

class Y { has @.y = (1, 2, 3).List }
is-deeply Y.new.y, [1, 2, 3], 'a List default list-assigns';

class I { has @.i = %(x => 1) }
is-deeply I.new.i, [x => 1], 'a Hash default flattens to its pairs';

class S { has @.s = 5 }
is-deeply S.new.s, [5], 'a scalar default becomes a one-element array';

class T { has @.t = (1, 2, 3) }
is-deeply T.new.t, [1, 2, 3], 'a parenthesised list default still works';

class B { has @.b = [1, 2, 3] }
is-deeply B.new.b, [1, 2, 3], 'a bracketed array default still works';

class V { has @.v }
is-deeply V.new.v, [], 'no default is an empty array';

class J { has @.j = 1, 2 ... 6 }
is-deeply J.new.j, [1, 2, 3, 4, 5, 6], 'a sequence default flattens';

class K { has @.k = Any }
is-deeply K.new.k, [Any], 'a type-object default becomes one element';

class N { has @.n = Nil }
is-deeply N.new.n, [Any], 'a Nil default becomes [Any]';

class Q { has @.q = <a b c> }
is-deeply Q.new.q, ['a', 'b', 'c'], 'a word-quote default flattens';

class Ty { has Int @.t2 = 1..3 }
is Ty.new.t2.join(','), '1,2,3', 'a typed @ attribute list-assigns its default';

# --- values supplied at construction time ---------------------------------
class F { has @.a }
is-deeply F.new(a => 5).a,           [5],       'a supplied scalar becomes one element';
is-deeply F.new(a => Any).a,         [Any],     'a supplied type object becomes one element';
is-deeply F.new(a => Nil).a,         [Any],     'a supplied Nil becomes [Any]';
is-deeply F.new(a => "x").a,         ['x'],     'a supplied Str becomes one element';
is-deeply F.new(a => (1..3)).a,      [1, 2, 3], 'a supplied Range flattens';
is-deeply F.new(a => (1, 2, 3).Seq).a, [1, 2, 3], 'a supplied Seq flattens';
is-deeply F.new(a => %(x => 1)).a,   [x => 1],  'a supplied Hash flattens to pairs';
is-deeply F.new(a => [1, 2]).a,      [1, 2],    'a supplied array flattens';
is-deeply F.new(a => ()).a,          [],        'a supplied empty list is empty';

# --- shaped attribute arrays keep their shape -----------------------------
class Sh { has @.g[3] }
my $sh = Sh.new;
$sh.g[1] = 9;
is $sh.g.elems, 3, 'a shaped @ attribute keeps its shape';

# --- a lazy/infinite default stays lazy -----------------------------------
class L { has @.l = (1..Inf).map(* * 2) }
is-deeply L.new.l[^3].List, (2, 4, 6), 'an infinite default is not eagerly reified';

# --- regression controls: `my @a = ...` is the rule being shared ----------
my @m1 = 1..3;
is-deeply @m1, [1, 2, 3], 'my @a = Range still works';
my @m2 = 5;
is-deeply @m2, [5], 'my @a = scalar still works';

# vim: ft=raku
