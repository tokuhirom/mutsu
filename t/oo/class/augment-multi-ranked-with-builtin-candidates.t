use Test;

# An `augment`ed `multi method` on a core type joins the dispatcher of the
# built-in method and is ranked with its candidates (#12751). A user candidate
# no narrower than a built-in one that binds the same arguments never takes the
# call (the built-in is declared first), and one added to an ancestor of a class
# that declares the method itself is not a candidate of that class's dispatcher.
plan 16;

use MONKEY-TYPING;

augment class Any {
    multi method first(Callable $c) { 'user-first-callable' }
    multi method first(Int $c) { 'user-first-int' }
    multi method first(Str $c) { 'user-first-str' }
    multi method head(Int $x) { 'user-head-int' }
    multi method head(Callable $x) { 'user-head-callable' }
    multi method sort(Int $x) { 'user-sort-int' }
    multi method sort(Callable $x) { 'user-sort-callable' }
    multi method tail(Int $x) { 'user-tail-int' }
    multi method grep(Callable $x) { 'user-grep-callable' }
    multi method elems(Int $x) { 'user-elems-int' }
}

# the built-in candidate takes the call: its parameter is no wider
is (1, 2, 3).first(* > 1), 2, 'first(Callable) is the built-in Callable:D candidate';
is <a b c>.first(/b/), 'b', 'first(Regex) is the built-in Regex:D candidate';

# the user candidate is narrower than the built-in `Mu` candidate
is (1, 2, 3).first(2), 'user-first-int', 'first(Int): the built-in Mu candidate is wider';
is <a b c>.first('b'), 'user-first-str', 'first(Str) likewise';

# List declares head, sort and tail itself, so an Any candidate is not in its dispatcher
is-deeply (1, 2, 3).head(2).list, (1, 2), 'List.head(Int) is List\'s own';
is-deeply (1, 2, 3).head(* - 1).list, (1, 2), 'List.head(Callable) too';
is-deeply (3, 1, 2).sort(* <=> *).list, (1, 2, 3), 'List.sort(Callable)';
is (1, 2, 3).tail(2).join(','), '2,3', 'List.tail(Int)';

# an Int has none of those: the Any dispatcher is the receiver's
is 5.head(2), 'user-head-int', 'Int.head(Int) is the augmented candidate';
is 5.sort(1), 'user-sort-int', 'Int.sort(Int) too';
is 5.tail(2), 'user-tail-int', 'Int.tail(Int) too';
is 5.elems(2), 'user-elems-int', 'Int.elems(Int)';

# the augmented candidate is still the one that wins when nothing built-in binds
is (1, 2, 3).grep(* > 1), 'user-grep-callable', 'grep(Callable): the built-in grep takes Mu, which is wider';
is (1, 2, 3).first(Int), 'user-first-int', 'first(Int) type object';

# the built-in stays reachable for what the augment does not take
is (5, 6, 7).first(* > 5), 6, 'a second call of first(Callable)';
is (1, 2, 3).sort.list.join(','), '1,2,3', 'sort without arguments';
