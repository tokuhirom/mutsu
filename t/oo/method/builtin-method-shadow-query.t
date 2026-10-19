use Test;

# A declaration of the program takes a call from the built-in method it
# shadows, whichever entry dispatches the call (ADR-11276 §2.3, one shadow
# query): a literal method name (the VM's CallMethod), a named receiver (its
# CallMethodMut lane), a quoted name, a dynamic name (the interpreter), and a
# hyper call (the native entry per element).

plan 30;

class A { method elems { 42 }; has $.uc = 'acc' }
my $a = A.new;
is A.new.elems, 42, 'a user method shadows Any.elems';
is $a.elems, 42, 'on a named receiver too';
my $m = 'elems';
is $a."$m"(), 42, 'through a dynamic name too';
is A.new."uc"(), 'acc', 'a public accessor shadows Cool.uc, quoted';
is A.new.uc, 'acc', 'and unquoted';
is $a.uc, 'acc', 'and on a named receiver';
is ($a, $a)».elems, (42, 42), 'and per element of a hyper call';

class B { method WHICH { 'b' } }
is B.new.WHICH, 'b', 'WHICH is an ordinary, overridable method';
my $wm = 'WHICH';
is B.new."$wm"(), 'b', 'through a dynamic name too';

class C { method WHAT { 'nope' } }
isa-ok C.new.WHAT, C, 'WHAT is a macro: a user method does not shadow it';
is C.new."WHAT"(), 'nope', 'except through a quoted call';

role R { method sum { 99 } }
is ([1, 2] but R).sum, 99, 'a role mixed in shadows List.sum';
my $mixed = [1, 2] but R;
is $mixed.sum, 99, 'on a named receiver too';

class D { has @.list = 1, 2 }
is D.new.list, [1, 2], 'an attribute accessor shadows Any.list';
class E { my @.items = 7, 8 }
is E.items, [7, 8], 'a class-scoped accessor answers on the type object';

class F { method gist { 'F!' } }
is F.new.gist, 'F!', 'a user gist';
like F.new.Str, /^'F<'\d+'>'$/, 'Str stays the built-in one';
is F.gist, 'F!', 'a user gist that accepts the type object answers it';

class H { multi method gist(H:D:) { 'H!' } }
is H.new.gist, 'H!', 'a definite-only gist answers an instance';
is H.gist, '(H)', 'and leaves the type object to Mu.gist';

class G is Int { method succ { 'gs' } }
is G.new(3).succ, 'gs', 'a user subclass of Int shadows Int.succ';
is G.new(3).pred, 2, 'and keeps the rest of Int';

my $five = 5 but role { method Str { 'five' } };
is ~$five, 'five', 'a role mixed into an Int shadows Int.Str';
is $five + 1, 6, 'and keeps its value';

class I { method FALLBACK($name, *@) { "fb:$name" } }
is I.new.flip, 'fb:flip', 'a Cool-only name reaches FALLBACK';
class J { method uc { 'ju' } }
is J.new.uc, 'ju', 'a user uc on a non-Cool class';
throws-like { J.new.flip }, X::Method::NotFound, 'a Cool-only name a plain class lacks';

use MONKEY-TYPING;
augment class Array { method shuffle-twice { 'aug' } }
is [3, 1].shuffle-twice, 'aug', 'an augment adds to a built-in type';
augment class Str { multi method lines(Str:D: |c) { 'never' } }
is "a\nb".lines, ('a', 'b'), 'a slurpy-only augmented multi ranks after the built-in';
is (1, 2, 3).map(* * 2), (2, 4, 6), 'an unshadowed built-in still answers';
