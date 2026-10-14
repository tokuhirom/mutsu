use Test;
use MONKEY-TYPING;
plan 9;

# An augmented `multi` joins the built-in method's candidates, and a candidate
# that is only a capture ranks after the built-in's positional one, so it is
# never picked for a call the built-in serves (#12751).
augment class Any {
    multi method map(|c) { 'user-map' }
    multi method grep(|c) { 'user-grep' }
    multi method first(|c) { 'user-first' }
}

is (1, 2).map(* + 1).List, (2, 3), 'List.map';
is 5.map(* + 1).List, (6,), 'Int.map';
is (1..3).map(* + 1).List, (2, 3, 4), 'Range.map';
is [3, 1, 2].map(* + 1).List, (4, 2, 3), 'Array.map';
is (3, 1, 2).map(* + 1, :label<x>).List, (4, 2, 3), 'a named argument does not change the ranking';
is (3, 1, 2).grep(* > 1).List, (3, 2), 'List.grep';
is (3, 1, 2).first(* > 1), 3, 'List.first';

# A user candidate with a positional parameter is not outranked: it is the
# narrower one for a matching argument.
class Local {
    multi method pick-it(|c) { 'slurpy' }
}
is Local.new.pick-it(1), 'slurpy', 'a user class with only user candidates is unaffected';

# A method with no built-in of that name keeps its user candidates.
augment class Any {
    multi method frobnicate(|c) { 'user-frobnicate' }
}
is 5.frobnicate, 'user-frobnicate', 'a new method of an augmented built-in class';
