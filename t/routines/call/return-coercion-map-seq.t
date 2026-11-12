use Test;

# A coercion return type reads a not-yet-run `.map`/`.grep` Seq's elements,
# not its still-empty seed (#17513: Red's `^column-values`, declared
# `--> Hash()`, answered `{}` and fixture rows lost their values).

plan 7;

sub to-hash(--> Hash()) { (1, 2).map: { $_ => $_ * 10 } }
is-deeply to-hash(), %(1 => 10, 2 => 20), '--> Hash() of a .map Seq';

sub to-array(--> Array()) { (1, 2, 3).map(* * 2) }
is-deeply to-array(), [2, 4, 6], '--> Array() of a .map Seq';

sub to-list(--> List()) { (1, 2, 3).grep(* > 1) }
is-deeply to-list(), (2, 3), '--> List() of a .grep Seq';

class Model {
    has @.columns = <a b>;
    method column-values(\model --> Hash()) { @!columns.map: { $_ => model } }
}
is-deeply Model.new.column-values(7), %(a => 7, b => 7), 'a method with a coercion return type';

sub empty-grep(--> Hash()) { (1, 2).grep(* > 5).map({ $_ => 1 }) }
is-deeply empty-grep(), %(), 'an empty pipeline coerces to an empty Hash';

sub gathered(--> Array()) { gather { take 1; take 2 } }
is-deeply gathered(), [1, 2], 'a gather still coerces';

sub plain(--> Seq) { (1, 2).map(* + 1) }
is plain().list, (2, 3), 'a plain Seq return type keeps the Seq';
