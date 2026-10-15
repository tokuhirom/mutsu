use Test;
use MONKEY-TYPING;

plan 12;

# The `multi method` candidates of a class (or an `augment`) join the
# candidates inherited from a built-in proto: a call none of the user's
# accepts reaches the built-in one (#12751).

class C { multi method map(Int $x) { 'c-int' } }
is C.new.map(5), 'c-int', 'the user candidate serves a call it accepts';
is C.new.map(*.so).List, (True,), 'a call it does not accept reaches Any.map';

class D { multi method elems(Int $x) { 'd-int' } }
is D.new.elems, 1, 'Any.elems';
is D.new.elems(1), 'd-int', 'the user candidate still wins for its own arguments';

class E { multi method say(Int $x) { 'e-int' } }
is E.new.say(1), 'e-int', 'Mu.say joined by a user candidate';

# An unrelated proto the receiver does not inherit stays an error.
class F { multi method frob(Int $x) { 'f-int' } }
throws-like { F.new.frob('a') }, X::Multi::NoMatch, 'no built-in of that name: no match';

augment class Any {
    multi method map(Int $x) { 'user-int' }
}
is (1, 2).map(* + 1).List, (2, 3), 'augmented Any.map: List receiver, a call the user candidate rejects';
is (1, 2).map(5), 'user-int', 'the augmented candidate serves its own call';
is 5.map(* + 1).List, (6,), 'Int receiver';
my class G {}
is G.new.map(*.so).List, (True,), 'a user-class instance';

augment class Any {
    multi method grep(Str $x) { 'user-str' }
}
is (3, 1, 2).grep("a"), 'user-str', 'augmented grep(Str)';
is (3, 1, 2).grep(* > 1).List, (3, 2), 'augmented grep: the built-in for a Callable';
